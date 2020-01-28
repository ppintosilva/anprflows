#' Get pretty time labels from breaks.
#'
#' @param time_breaks Trimmed flows tibble.
#' @importFrom magrittr %>%
get_time_labels <- function(time_breaks) {
  time_labels <- c(
    time_breaks[1] %>% format("%y-%m-%d %H:%M"),
    time_breaks[2:(length(time_breaks)-1)] %>% format("%H:%M"),
    tail(time_breaks,1) %>% format("%y-%m-%d %H:%M")
  )
  return(time_labels)
}
