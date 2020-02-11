#' Plot flow network.
#'
#' @param flows_od Summarised or windowed od flows tibble.
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
#' @param node_size Node size
#' @param node_color Node color
#' @param aes_node_label Optionally label nodes according to this aesthetic
#' @param node_label_nudge_y Shift amount applied node labels on the y axis
#' @param node_label_nudge_x Shift amount applied node labels on the x axis
#'
#' @export
#'
plot_small_network <- function(
  flows_od,
  ignore_source_sink = FALSE,
  num_accuracy = .1,
  num_scale = 1,
  graph_layout = "auto",
  aes_edge_color = "",
  aes_edge_label = "flow",
  edge_angle_calc = 'along',
  edge_label_dodge = ggplot2::unit(2.5, 'mm'),
  edge_arrow = ggplot2::arrow(length = ggplot2::unit(1.5, 'mm')),
  edge_start_cap = ggraph::circle(1, 'mm'),
  edge_end_cap = ggraph::circle(2, 'mm'),
  edge_width = 1,
  edge_alpha = .75,
  node_size = 4,
  node_color = munsell::mnsl("5BG 6/4"),
  aes_node_label = "name",
  node_label_nudge_y = 0.1,
  node_label_nudge_x = 0
) {

  stop_if_multiple_time_steps(flows_od)

  graph <-
    flows_od %>%
    ungroup() %>%
    {
      if(ignore_source_sink) {
        filter(., .data$o != "SOURCE", .data$d != "SINK")
      } else .
    } %>%
    {
      if(aes_edge_label != "") {
        mutate(., label = scales::number(!!sym(aes_edge_label),
                                         accuracy = num_accuracy,
                                         scale = num_scale))
      } else .
    } %>%
    {
      if(tibble::has_name(., 't')) {
        select(., -.data$t)
      } else .
    } %>%
    rename(from = !!sym("o"), to = !!sym("d")) %>%
    tidygraph::as_tbl_graph()

  label_mapping <- ifelse(aes_edge_label != "", "label", "")

  graph %>%
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
    geom_node_point(
      size = node_size,
      colour = node_color
    ) +
    {
      if(aes_node_label != "") {
        geom_node_text(
          ggplot2::aes(
            label = !!sym(aes_node_label)
          ),
          nudge_y = node_label_nudge_y,
          nudge_x = node_label_nudge_x
        )
      }
    }
}
