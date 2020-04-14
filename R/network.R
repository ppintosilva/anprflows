#' Create a tidygraph from flows and remove spurious edges in the process.
#'
#' @param flows Asymptotic od flows
#' @param spurious_if_below Remove edges whose attributes are lower than
#' the corresponding threshold.
#' @param label_subgraphs Whether to label subgraphs as a result of removing
#' spurious edges.
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

  # save levels so that we can restore them later
  location_levels <- levels(flows$o)

  filtered_flows %>%
    as_tbl_graph() %>%
    activate("nodes") %>%
    {
      if(label_subgraphs) {
        left_join(
          .,
          subgraph_labels(.),
          by = c("name" = "name")
        ) %>% mutate(subgraph = factor(.data$subgraph))
      } else .
    } %>%
    {
      if(names_as_factors & !is.null(node_levels)) {
        mutate(., name = factor(.data$name, levels = node_levels))
      } else if(names_as_factors) {
        mutate(., name = factor(.data$name))
      } else .
    } %>%
  mutate(name = forcats::fct_relevel(.data$name, location_levels))
}

#' Label nodes according to the subgraph, if any, which they belong in.
#'
#' @param network A flow network
#' @param include_source_sink Whether to include source and sink nodes in each
#' subgraph.
#'
#' @return [list]
#'
#' @export
#'
get_subgraphs <- function(network, include_source_sink = TRUE) {
  distinct_labels <-
    network %>%
    activate("nodes") %>%
    as_tibble() %>%
    distinct(.data$subgraph) %>%
    filter(!is.na(.data$subgraph)) %>%
    pull(.data$subgraph)

  get_subgraph <- function(label)
    network %>% activate("nodes") %>%
    {
      if(include_source_sink)
        filter(., .data$subgraph == label | is.na(.data$subgraph))
      else
        filter(., .data$subgraph == label)
    }

  lapply(distinct_labels, get_subgraph)
}


#' Label nodes according to the subgraph, if any, which they belong in.
#'
#' @param network A flow network
#'
#' @return [tbl_graph]
#'
#' @export
#'
subgraph_labels <- function(network) {
  # make sure source and sink are deleted before finding sub-graphs
  communities <-
    network %>%
    filter(! .data$name %in% c("SOURCE", "SINK")) %>%
    igraph::decompose.graph()

  1:length(communities) %>%
  lapply(function(x)
          communities[[x]] %>%
          as_tbl_graph() %>%
          activate("nodes") %>%
          as_tibble() %>%
          mutate(subgraph = x)) %>%
    bind_rows()
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

#' Get nodes tibble from flow network
#'
#' @param network A flow network
#'
#' @return [tibble][tibble::tibble-package]
#'
#' @export
#'
flow_nodes <- function(network) {
  network %>%
    activate("nodes") %>%
    as_tibble() %>%
    tibble::rownames_to_column(var = "i") %>%
    mutate(i = as.integer(.data$i))
}

#' Get edges tibble from flow network
#'
#' @param network A flow network
#' @param nodes tibble of flow nodes, or a subset of these
#'
#' @return [tibble][tibble::tibble-package]
#'
#' @export
#'
flow_edges <- function(network, nodes = NULL) {
  if(is.null(nodes))
    nodes <- flow_nodes(network)

  network %>%
    activate("edges") %>%
    as_tibble() %>%
    inner_join(
      nodes %>% rename(o = .data$name),
      by = c("from" = "i")
    ) %>%
    inner_join(
      nodes %>% rename(d = .data$name),
      by = c("to" = "i"),
      suffix = c(".o", ".d")
    ) %>%
    select(.data$o, .data$d, everything())
}
