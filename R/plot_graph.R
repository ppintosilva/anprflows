#' Plot flow network.
#'
#' @param flows_od Summarised or windowed od flows tibble.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom tibble has_name
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
  edge_arrow = arrow(length = ggplot2::unit(1.5, 'mm')),
  edge_start_cap = circle(1, 'mm'),
  edge_end_cap = circle(2, 'mm'),
  edge_width = 1,
  edge_alpha = .75,
  node_size = 4,
  node_color = munsell::mnsl("5BG 6/4"),
  aes_node_label = "name",
  node_label_nudge_y = 0.1,
  node_label_nudge_x = 0
) {

  if(has_name(flows_od, 't')) {
    if(length(unique(flows_od$t)) > 1) {
      stop(paste0(
      "Input flow data must be summarised or windowed ",
      "(length(unique(flows_od$t)) == 1)"))
    }
  }

  graph <-
    flows_od %>%
    ungroup() %>%
    {
      if(ignore_source_sink) {
        filter(., o != "SOURCE", d != "SINK")
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
      if(has_name(., 't')) {
        select(., -t)
      } else .
    } %>%
    rename(from = o, to = d) %>%
    tidygraph::as_tbl_graph()

  label_mapping <- ifelse(aes_edge_label != "", "label", "")

  graph %>%
    ggraph(layout = graph_layout) +
    geom_edge_fan(
      aes(
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
          aes(
            label = !!sym(aes_node_label)
          ),
          nudge_y = node_label_nudge_y,
          nudge_x = node_label_nudge_x
        )
      }
    }
}
