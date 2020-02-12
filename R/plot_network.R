#' Plot flow network.
#'
#' @param network Flow network.
#' @param ignore_source_sink Whether to ignore source and sink nodes.
#' @param num_accuracy Number format accuracy of edge labels.
#' @param num_scale Multiplicative factor applied to edge labels.
#' @param graph_layout ggraph layout parameter
#' @param aes_edge_color Optionally color edges according to this aesthetic
#' @param aes_edge_label Optionally label edges according to this aesthetic
#' @param edge_angle_calc Edges label angle parameter
#' @param edge_label_dodge Edges label dodge parameter
#' @param edge_arrow Edge arrow
#' @param edge_start_cap Arrow distance from starting node
#' @param edge_end_cap Arrow distance from ending node
#' @param edge_width Edge width
#' @param edge_alpha Edge alpha
#' @param label_size Node label size
#' @param aes_node_label Label nodes aesthetic
#' @param aes_node_fill Fill nodes aesthetic
#' @param node_label_color Font color of node labels
#' @param node_label_palette Fill color brewer palette
#'
#' @export
#'
plot_small_network <- function(
  network,
  ignore_source_sink = FALSE,
  num_accuracy = .1,
  num_scale = 1,
  graph_layout = "auto",
  aes_edge_color = "",
  aes_edge_label = "flow",
  edge_angle_calc = 'along',
  edge_label_dodge = ggplot2::unit(2.5, 'mm'),
  edge_arrow = ggplot2::arrow(length = ggplot2::unit(3, 'mm')),
  edge_start_cap = ggraph::circle(5, 'mm'),
  edge_end_cap = ggraph::circle(5, 'mm'),
  edge_width = 1,
  edge_alpha = .75,
  label_size = 5,
  aes_node_label = "name",
  aes_node_fill = "name",
  node_label_color = "black",
  node_label_palette = "Set2"
) {

  stopifnot("tbl_graph" %in% (class(network)))

  if(ignore_source_sink) {
    network <-
      network %>%
      igraph::delete_vertices(c("SOURCE", "SINK")) %>%
      as_tbl_graph()
  }

  if(aes_edge_label != "") {
    network <-
      network %>%
      activate("edges") %>%
      mutate(label = scales::number(!!sym(aes_edge_label),
                                    accuracy = num_accuracy,
                                    scale = num_scale))
  }

  label_mapping <- ifelse(aes_edge_label != "", "label", "")

  network %>%
    ggraph(layout = graph_layout) +
    geom_edge_fan(
      ggplot2::aes(
        color = !!sym(aes_edge_color),
        label = !!sym(label_mapping)
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
        label = !!sym(aes_node_label),
        fill = !!sym(aes_node_fill)
      ),
      colour = node_label_color,
      size = label_size
    ) +
    ggplot2::scale_fill_brewer(palette = node_label_palette) +
    ggplot2::guides(fill = "none")
}
