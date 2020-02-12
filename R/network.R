#' Create a tidygraph from flows and remove spurious edges in the process.
#'
#' @param flows Asymptotic od flows
#' @param spurious_if_below Remove edges whose attributes are lower than
#' the corresponding threshold.
#' @param label_subgraphs Whether to label subgraphs as a result of removing
#' spurious edges.
#' @param subgraphs_ignore_source_sink Whether to ignore source and sink
#' nodes in label calculation.
#' @param names_as_factors Whether to convert node names to factor.
#' @param node_levels Optionally set the levels of node factor.
#'
#' @return [tbl_graph]
#'
#' @export
#'
flow_network <- function(
  flows,
  spurious_if_below = c("rate_o" = .1, "rate_d" = .1),
  label_subgraphs = TRUE,
  subgraphs_ignore_source_sink = TRUE,
  names_as_factors = TRUE,
  node_levels = NULL
) {

  # If column 't' exists it should have a single distinct value
  stop_if_multiple_time_steps(flows)

  if(has_name(flows, 't')) {
    flows <- flows %>% select(-t)
  }

  if(!is.null(spurious_if_below)) {
    filtered_flows <-
      lapply(
        names(spurious_if_below),
        function(x) flows %>% filter(!!sym(x) > spurious_if_below[x])
      ) %>%
      bind_rows() %>%
      distinct(.data$o,.data$d, .keep_all = TRUE)
  } else {
    filtered_flows <- flows
  }

  filtered_flows %>%
    as_tbl_graph() %>%
    activate("nodes") %>%
    {
      if(label_subgraphs) {
        left_join(
          .,
          get_subgraphs(., subgraphs_ignore_source_sink),
          by = c("name" = "name")
        )
      }
    } %>%
    {
      if(names_as_factors & !is.null(node_levels)) {
        mutate(., name = factor(.data$name, levels = node_levels))
      } else if(names_as_factors) {
        mutate(., name = factor(.data$name))
      }
    }
}


#' Label nodes according to the subgraph, if any, which they belong in.
#'
#' @param network A flow network
#' @param ignore_source_sink Whether to ignore source and sink nodes in label
#' calculation.
#'
#' @return [tbl_graph]
#'
#' @export
#'
get_subgraphs <- function(network, ignore_source_sink = TRUE) {
  communities <-
    network %>%
    {
      if(ignore_source_sink) {
        igraph::delete.vertices(., c("SOURCE", "SINK"))
      }
    } %>%
    igraph::decompose.graph()

  1:length(communities) %>%
  lapply(function(x)
          communities[[x]] %>%
          as_tbl_graph() %>%
          activate("nodes") %>%
          as_tibble() %>%
          mutate(subgraph = x)) %>%
    bind_rows() %>%
    mutate(
      subgraph = ifelse(.data$name == "SOURCE" | .data$name == "SINK",
                        NA, .data$subgraph)
    )
}


#' Get the subset of the flow network which contains all nodes that are
#' neighbors to a list of nodes.
#'
#'
#' @param network Flow network
#' @param nodes Character vector of node names
#'
#' @export
#'
get_neighbors <- function(
  network, nodes
) {
  # warning: removing nodes from input list if not present in graph
  network_nodes <-
    network %>% tidygraph::activate("nodes") %>%
    as_tibble() %>% pull(.data$name)

  node_diff <- setdiff(nodes, network_nodes)
  if(length(node_diff) > 0) {
    warning(paste0("Ignoring nodes ", node_diff,
                   " which can not be found in the network."))
    nodes <- setdiff(nodes, node_diff)
  }

  stopifnot(length(nodes) > 0)

  # Get neighbors of each node as a list
  neighbors <-
    nodes %>%
    lapply(
      function(x) igraph::neighbors(network, x, "all")
    )

  # Union of all neighbors
  all_nodes <- do.call(igraph::union, c(neighbors))

  # append the original nodes
  all_nodes <- igraph::union(
    all_nodes,
    igraph::V(network)[igraph::V(network)$name %in% nodes]
  )

  # get graph that contains these nodes
  igraph::induced_subgraph(
    network,
    all_nodes,
    impl = 'copy_and_delete'
  ) %>%
    as_tbl_graph()
}
