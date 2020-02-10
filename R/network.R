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
  spurious_thresholds = c("rate_o" = .1, "rate_d" = .1),
  label_subgraphs = TRUE
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

  filtered_flows %>%
    as_tbl_graph() %>%
    {
      if(label_subgraphs) {
        activate(., nodes) %>%
          left_join(
            get_subgraphs(.),
            by = c("name" = "name")
          )
      }
    }
}


#' Label nodes according to the subgraph, if any, that they belong in.
#'
#' @param network A flow network
#'
#' @export
#'
get_subgraphs <- function(network) {
  communities <- igraph::decompose.graph(network)

  1:length(communities) %>%
  lapply(function(x)
          communities[[x]] %>%
          as_tbl_graph() %>%
          activate(nodes) %>%
          as_tibble() %>%
          mutate(subgraph = x)) %>%
    bind_rows() %>%
    mutate(
      subgraph = ifelse(.data$name == "SOURCE" | .data$name == "SINK",
                        -1, .data$subgraph)
    )
}
