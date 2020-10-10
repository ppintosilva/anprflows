#' Crop spatial features based on od pairs.
#'
#' @param pairs OD pairs tibble.
#' @param spatial List of spatial features.
#' @param arterial_highway Only keep arterial edges with this value in the
#' highway attribute.
#' @param bbox_margin Spatial margin added to flows bounding box.
#'
#' @return [list]
#' @export
#'
crop_spatial <- function(
  pairs,
  spatial,
  arterial_highway = "residential",
  bbox_margin = c(-500,-500,500,500)
) {
  col1 <- sym(names(pairs)[1])
  col2 <- sym(names(pairs)[2])
  # get unique locations observed in the data
  observed_locations <- union(pairs[,col1], pairs[,col2])

  # subset locations by existing flows
  locations <-
    spatial$locations %>%
    filter(.data$id %in% observed_locations)

  # od combinations
  od_combinations <-
    pairs %>%
    filter({{ col1 }} != "SOURCE", {{ col2 }} != "SINK") %>%
    distinct({{ col1 }}, {{ col2 }})

  pairs <-
    suppressWarnings(
      inner_join(
        spatial$pairs,
        od_combinations,
        by = c("o" = rlang::as_string(col1), "d" = rlang::as_string(col2))
      )
    )

  # intersection primary and arterial with flows (zoom in)
  bbox <-
    pairs %>%
    pull(.data$geometry) %>%
    sf::st_bbox() +
    bbox_margin

  # looking for arterial network data in
  #   spatial$arterial and spatial$arterial$edges
  if(is.null(spatial$arterial$edges)) {
    arterial <- try_st_crop(spatial$arterial, bbox, "arterial network")
  } else {
    arterial <- try_st_crop(spatial$arterial$edges, bbox, "arterial network")
  }

  arterial <-
    arterial %>%
    {
      if(!is.null(arterial_highway))
        filter(., .data$highway == arterial_highway)
      else .
    }

  # looking for primary network data in
  #   spatial$primary and spatial$primary$edges
  if(is.null(spatial$primary$edges)) {
    primary <- try_st_crop(spatial$primary, bbox, "primary network")
  } else {
    primary <- try_st_crop(spatial$primary$edges, bbox, "primary network")
  }

  if('amenities' %in% names(spatial)) {
    amenities <- try_st_crop(spatial$amenities, bbox, "amenities")
  } else {
    amenities <- NULL
  }

  return(
    list(
      "locations" = locations,
      "pairs" = pairs,
      "primary" = primary,
      "arterial" = arterial,
      "amenities" = amenities
    )
  )
}
