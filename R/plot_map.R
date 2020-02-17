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
          flows %>% filter(.data$o != "SOURCE", .data$d != "SINK"),
          spatial$pairs
        )
      )
  }

  # Plot time
  ggplot2::ggplot() +
    # primary network
    {
      if(add_primary) {
        ggplot2::geom_sf(
          data = primary,
          mapping = ggplot2::aes(geometry = .data$geometry),
          color = color_primary,
          size = size_primary
        )
      }
    } +
    {
      if(add_arterial) {
        ggplot2::geom_sf(
          data = arterial,
          mapping = ggplot2::aes(geometry = .data$geometry),
          color = color_arterial,
          size = size_primary
        )
      }
    } +
    {
      if(add_paths & aes_color_flows == "") {
        ggplot2::geom_sf(
          data = pairs,
          mapping = ggplot2::aes(geometry = .data$geometry),
          color = color_paths,
          size = size_paths
        )
      }
    } +
    {
      if(add_paths & aes_color_flows != "") {
        ggplot2::geom_sf(
          data = pairs,
          mapping = ggplot2::aes(
            geometry = .data$geometry,
            color = !!sym(aes_color_flows)),
          size = size_paths
        )
      }
    } +
    {
      if(add_locations) {
        ggplot2::geom_sf(
          data = locations,
          mapping = ggplot2::aes(
            geometry = .data$geometry,
            fill = !!sym(aes_color_locations)),
          color = ifelse(aes_color_locations == "", color_locations, NA),
          size = size_locations
        )
      }
    } +
    {
      if(aes_color_locations != "") {
        ggplot2::scale_fill_brewer(palette = locations_palette,
                                   name = "location")
      }
    } +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    )
}


#' Plot a spatial map for every od pair in a given flow network.
#'
#' @param spatial list of spatial features
#' @param network flow network
#' @param ... parameters to be passed to plot_map
#'
#' @export
#'
plot_map_pairs <- function(
  spatial, network,
  ...
) {

  nodes <- network %>% activate("nodes") %>% as_tibble() %>%
    filter(.data$name != "SOURCE", .data$name != "SINK") %>%
    tibble::rownames_to_column(var = "i") %>%
    mutate(i = as.integer(.data$i),
           name = as.character(.data$name)) %>%
    select(.data$i, .data$name)

  nnodes <- nrow(nodes)

  edges <- network %>% activate("edges") %>% as_tibble() %>%
    inner_join(nodes %>% rename(o = .data$name), by = c("from" = "i")) %>%
    inner_join(nodes %>% rename(d = .data$name), by = c("to" = "i")) %>%
    filter(.data$o != "SOURCE", .data$d != "SINK")

  xy_limits <- sf::st_bbox(spatial$primary)
  x_limits <- xy_limits[c(1,3)]
  y_limits <- xy_limits[c(2,4)]

  plot_list <- list()

  # for edge in the network that does not contain SOURCE/SINK
  for(i in 1:nnodes) {
    for(j in 1:nnodes) {
      index <- (i-1) * nnodes + j

      candidate_edge <- edges %>% filter(.data$from == j & .data$to == i)

      # if i,j is an edge (1 in adjacency matrix) then build plot
      if(nrow(candidate_edge) > 0) {
        odpair <- candidate_edge
        od_nodes <- union(odpair$o, odpair$d)

        # tmp var
        subspatial <- spatial

        subspatial$locations <-
          subspatial$locations %>%
          filter(.data$id %in% od_nodes)

        subspatial$pairs <-
          subspatial$pairs %>%
          inner_join(odpair, by = c("o" = "o", "d" = "d"))

        # return a map for each edge in the network
        plot_list[[index]] <- plot_map(subspatial, ...) +
          ggplot2::theme(axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank())

      }
      else {
        plot_list[[index]] <- ggplot2::ggplot() +
          ggplot2::coord_sf(xlim = x_limits, ylim = y_limits) +
          ggplot2::theme_void()
      }
    }
  }

  # fill diagonal and missing edges with empty plots
  GGally::ggmatrix(
    plot_list, nrow = nnodes, ncol = nnodes,
    yAxisLabels = nodes %>% pull(.data$name),
    xAxisLabels = nodes %>% pull(.data$name)
  )
}
