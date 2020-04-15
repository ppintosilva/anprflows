#' Plot flow network.
#'
#' @param network Flow network.
#' @param include_source_sink Whether to include source and sink nodes.
#' @param num_accuracy Number format accuracy of edge labels.
#' @param num_scale Multiplicative factor applied to edge labels.
#' @param graph_layout ggraph layout parameter
#'
#' @param edge_color Optionally color edges according to this aesthetic
#' @param edge_label Optionally label edges according to this aesthetic
#' @param node_label Label nodes aesthetic
#' @param node_fill Fill nodes aesthetic
#'
#' @param edge_angle_calc Edges label angle parameter
#' @param edge_label_dodge Edges label dodge parameter
#' @param edge_arrow Edge arrow
#' @param edge_start_cap Arrow distance from starting node
#' @param edge_end_cap Arrow distance from ending node
#' @param edge_width Edge width
#' @param edge_alpha Edge alpha
#'
#' @param label_size Node label size
#' @param label_color Font color of node labels
#' @param source_sink_label_fill Special fill color for source and sink nodes.
#'
#' @export
#'
plot_small_network <- function(
  network,
  include_source_sink = TRUE,
  graph_layout = "sugiyama",
  # aesthetics
  edge_color = NULL,
  edge_label = NULL,
  node_label = .data$name,
  node_fill = .data$subgraph,
  # edge beautifications
  edge_arrow = ggplot2::arrow(length = ggplot2::unit(3, 'mm')),
  edge_start_cap = ggraph::circle(5, 'mm'),
  edge_end_cap = ggraph::circle(5, 'mm'),
  edge_angle_calc = 'along',
  edge_label_dodge = ggplot2::unit(2.5, 'mm'),
  edge_width = 1,
  edge_alpha = .75,
  num_accuracy = .1,
  num_scale = 1,
  # label beautifications
  label_size = 5,
  label_color = "black",
  source_sink_label_fill = "grey70"
) {

  node_label <- enquo(node_label)
  node_fill  <- enquo(node_fill)
  edge_color <- enquo(edge_color)
  edge_label <- enquo(edge_label)

  stopifnot("tbl_graph" %in% (class(network)))

  if(!include_source_sink) {
    network <- network %>% activate("nodes") %>%
      filter(! .data$name %in% c("SOURCE", "SINK"))
  }

  if(! rlang::quo_is_null(edge_label)) {
    network <-
      network %>%
      activate("edges") %>%
      mutate(label = scales::number(!! edge_label,
                                    accuracy = num_accuracy,
                                    scale = num_scale))
    edge_label <- sym("label")
  } else {
    edge_label <- quo(NULL)
  }

  network %>%
    ggraph(layout = graph_layout) +
    geom_edge_fan(
      ggplot2::aes(
        color = !! edge_color,
        label = !! edge_label
      ),
      angle_calc = edge_angle_calc,
      label_dodge = edge_label_dodge,
      arrow = edge_arrow,
      start_cap = edge_start_cap,
      end_cap = edge_end_cap,
      edge_width = edge_width,
      edge_alpha = edge_alpha
    ) +
    geom_node_label(
      ggplot2::aes(
        label = !! node_label,
        fill =  !! node_fill
      ),
      colour = label_color,
      size = label_size
    ) +
    ggplot2::scale_fill_brewer(type = "qual",
                               na.value = source_sink_label_fill)
}

#' Plot flow network.
#'
#' @param network Flow network.
#' @param ... Parameters passed to plot_small_network
#'
#' @export
#'
small_network_plots <- function(
  network, ...
) {

  subG <- get_subgraphs(network)

  pls <- lapply(
    subG,
    plot_small_network,
    ...)

  return(pls)
}
