#' Get pretty time labels from breaks.
#'
#' @param time_breaks Trimmed flows tibble.
#'
get_time_labels <- function(time_breaks) {
  time_labels <- c(
    time_breaks[1] %>% format("%y-%m-%d %H:%M"),
    time_breaks[2:(length(time_breaks)-1)] %>% format("%H:%M"),
    tail(time_breaks,1) %>% format("%y-%m-%d %H:%M")
  )
  return(time_labels)
}

#' Get first element greater than number in vector.
#'
#' @param vec Numeric.
#' @param number Numeric of length 1.
first_element_greater <- function(vec, number){
  el <- vec[vec > number][1]
  ifelse(is.na(el), vec[length(vec)], el)
}


#' Check whether time column exists and throw error
#'
#' @param flows_od OD flows tibble.
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
