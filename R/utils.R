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
#'
#' @return vector of corresponding periods# of the day (categorical)
#'
#' @export
#'
dayperiod <- function(t) {
  dplyr::case_when(
      lubridate::hour(t) >= 21 | lubridate::hour(t) < 6 ~ "night",
      lubridate::hour(t) >= 6  & lubridate::hour(t) < 10 ~ "morning",
      lubridate::hour(t) >= 10 & lubridate::hour(t) < 16 ~ "day",
      lubridate::hour(t) >= 16 & lubridate::hour(t) < 21 ~ "afternoon"
    )
}

#' Transform a vector of n elements (1,2,3) into a vector of consecutive
#' character tuples of length n-1 ("1-2","2-3").
#'
#' @param v vector
#' @param unite_sep character separator to unite consecutive elements into tuples
#'
as_tuples <- function(v, unite_sep = "-") {
  v %>%
    enframe(name = NULL, value = "level") %>%
    mutate(level_lag1 = lead(.data$level, 1)) %>%
    # remove last row
    slice(1:(dplyr::n()-1)) %>%
    unite("level", .data$level, .data$level_lag1, sep = unite_sep) %>%
    pull(.data$level)
}
