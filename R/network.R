#' Create a tidygraph from flows and remove spurious edges in the process.
#'
#' @param flows Asymptotic od flows
#' @param spurious_thresholds Remove edges whose attributes are lower than
#' the corresponding threshold.
#'
#' @export
#'
flow_network <- function(
  flows,
  spurious_thresholds = c("rate_o" = .1, "rate_d" = .1)
) {

  # If column 't' exists it should have a single distinct value
  stop_if_multiple_time_steps(flows)

  if(!is.null(spurious_thresholds)) {
    filtered_flows <-
      lapply(
        names(spurious_thresholds),
        function(x) flows %>% filter(!!sym(x) > spurious_thresholds[x])
      ) %>%
      bind_rows() %>%
      distinct(.data$o,.data$d, .keep_all = TRUE)
  } else {
    filtered_flows <- flows
  }

  filtered_flows %>% as_tbl_graph()
}
