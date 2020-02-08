# Default color ---------------------------

cameras_col <- "N 3/0"
flows_col <- "5YR 6/6"
primary_col <- "5BG 6/4"
arterial_col <- "5BG 8/2"
polygon_col <- "N 9/0"

# Functions ---------------------------

#' Plot traffic demand over time, grouped by location.
#'
#' @param spatial List of spatial features.
#' @param flows Optional summarised flows$l tibble.
#' @param add_primary Whether to plot primary network.
#' @param add_arterial Whether to plot arterial network.
#' @param add_paths Whether to plot shortest paths between locations.
#' @param add_locations Whether to plot observed locations.
#' @param color_primary Color of primary network.
#' @param color_arterial Color of arterial network.
#' @param color_locations Color of locations.
#' @param color_paths Color of paths.
#' @param size_primary Line size of the primary network.
#' @param size_arterial Line size of the arterial network.
#' @param size_paths Line size of pahts.
#' @param size_locations Size of locations.
#' @param locations_palette Brewer palette for locations when used together
#' with aes_color_locations.
#' @param aes_color_locations Color aesthetic to use for locations as
#'  a character.
#' @param aes_color_flows Color aesthetic to use for paths as a character.
#'
#' @export
plot_map <- function(
  spatial,
  flows = NULL,
  add_primary = TRUE, add_arterial = FALSE,
  add_paths = TRUE, add_locations = TRUE,
  color_primary = munsell::mnsl(primary_col),
  color_arterial = munsell::mnsl(arterial_col),
  color_locations = munsell::mnsl(cameras_col),
  color_paths = munsell::mnsl(flows_col),
  size_primary = .2,
  size_arterial = .1,
  size_paths = .5,
  size_locations = 3,
  aes_color_locations = "",
  locations_palette = "Set2",
  aes_color_flows = ""
) {

  # If elses applied to data frames
  if(is.null(spatial$primary$edges)) {
    primary <- spatial$primary
  } else {
    primary <- spatial$primary$edges
  }

  if(is.null(spatial$arterial$edges)) {
    arterial <- spatial$arterial
  } else {
    arterial <- spatial$arterial$edges
  }

  if(aes_color_locations == "") {
    locations <- spatial$locations
  } else {
    locations <- spatial$locations %>% sf::st_buffer(size_locations * 10)
  }

  if(aes_color_flows == "") {
    pairs <- spatial$pairs
  } else if("geometry" %in% names(flows)){
    pairs <- flows
  } else {
    pairs <-
      suppressWarnings(
        inner_join(
          flows %>% filter(o != "SOURCE", d != "SINK"),
          spatial$pairs
        )
      )
  }

  # Plot time
  ggplot() +
    # primary network
    {
      if(add_primary) {
        geom_sf(
          data = primary,
          mapping = aes(geometry = geometry),
          color = color_primary,
          size = size_primary
        )
      }
    } +
    {
      if(add_arterial) {
        geom_sf(
          data = arterial,
          mapping = aes(geometry = geometry),
          color = color_arterial,
          size = size_primary
        )
      }
    } +
    {
      if(add_paths & aes_color_flows == "") {
        geom_sf(
          data = pairs,

          mapping = aes(geometry = geometry),
          color = color_paths,
          size = size_paths
        )
      }
    } +
    {
      if(add_paths & aes_color_flows != "") {
        geom_sf(
          data = pairs,
          mapping = aes(geometry = geometry,
                        color = !!sym(aes_color_flows)),
          size = size_paths
        )
      }
    } +
    {
      if(add_locations) {
        geom_sf(
          data = locations,
          mapping = aes(geometry = geometry,
                        fill = !!sym(aes_color_locations)),
          color = ifelse(aes_color_locations == "", color_locations, NA),
          size = size_locations
        )
      }
    } +
    {
      if(aes_color_locations != "") {
        scale_fill_brewer(palette = locations_palette, name = "location")
      }
    } +
    theme(
      panel.grid = element_blank(),
      axis.line = element_blank()
    )

}
