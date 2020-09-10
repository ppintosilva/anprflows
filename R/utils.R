#' Get pretty time labels from breaks. Full format is applied to generate
#' the first two labels, whilst partial format is applied to all labels in
#' between.
#'
#' @param datetime_breaks input datetime breaks
#' @param full_datetime_format datetime format applied to first and last breaks
#' @param partial_datetime_format datetime format applied to elements in between
#'
get_datetime_labels <- function(
  datetime_breaks,
  full_datetime_format = "%y-%m-%d %H:%M",
  partial_datetime_format = "%H:%M"
) {
  b <- datetime_breaks

  if(length(datetime_breaks) > 2) {
    l <- c(
      b[1] %>% format(full_datetime_format),
      b[2:(length(b)-1)] %>%
        format(partial_datetime_format),
      tail(b,1) %>% format(full_datetime_format)
    )
  } else {
    l <- b %>% format(full_datetime_format)
  }
  return(l)
}

#' Get first element greater than number in vector.
#'
#' @param vec numeric
#' @param number numeric of length 1
first_element_greater <- function(vec, number){
  el <- vec[vec > number][1]
  ifelse(is.na(el), vec[length(vec)], el)
}


#' Check whether time column exists and throw error
#'
#' @param flows_od OD flows tibble
stop_if_multiple_time_steps <- function(flows_od) {
  # If column 't' exists it should have a single distinct value
  if(tibble::has_name(flows_od, 't')) {
    if(length(unique(flows_od$t)) > 1) {
      stop(paste0(
        "Input flow data must be summarised or windowed ",
        "(length(unique(flows_od$t)) == 1)"))
    }
  }
}

#' Get an empty ggplot
blank_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(size = .2, colour = "grey30"))
}

#' Categorise time into its corresponding period of the day
#'
#' @param t time vector
#' @param bounds daily time boundaries
#'
#' @return vector of corresponding periods# of the day (categorical)
#'
#' @export
#'
dayperiod <- function(t, bounds = c(6,10,16,21)) {
  dplyr::case_when(
    lubridate::hour(t) >= bounds[4] | lubridate::hour(t) < bounds[1] ~ "night",
    lubridate::hour(t) >= bounds[1] & lubridate::hour(t) < bounds[2] ~ "morning",
    lubridate::hour(t) >= bounds[2] & lubridate::hour(t) < bounds[3] ~ "day",
    lubridate::hour(t) >= bounds[3] & lubridate::hour(t) < bounds[4] ~ "afternoon"
  )
}

#' Check whether time column exists and throw error
#'
#' @param sf_tibble a simple features tibble
#' @param bbox a st_bbox object
#' @param sf_name the name of the tibble to be used if st_crop fails
try_st_crop <- function(sf_tibble, bbox, sf_name) {
  try(
    cropped_sf <- suppressWarnings(sf::st_crop(sf_tibble, bbox))
  )

  if(cropped_sf %>% is.null()) {
    stop(paste0("GDAL threw an error while attempting to crop the '",
                sf_name, "' tibble. ",
                "Please adjust the bbox margins and try again."))
  }
  else {
    return(cropped_sf)
  }
}
