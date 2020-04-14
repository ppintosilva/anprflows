# # ---- Test different combinations of slice and raw/processed flow networks
#
p_asympt_no_source_sink_simple <-
  plot_small_network(G1_asympt,
                     include_source_sink = FALSE)

p_asympt_no_source_sink <-
  plot_small_network(G1_asympt,
                     num_scale = 0.001,
                     node_fill = NULL,
                     edge_label = flow,
                     include_source_sink = FALSE)

p_asympt_no_spurious <-
  plot_small_network(G1_asympt_no_spurious,
                     num_scale = 0.001,
                     edge_label = flow,
                     include_source_sink = TRUE)

p_8am_slice <-
  plot_small_network(G1_8am_slice,
                     num_accuracy = NULL,
                     edge_label = flow,
                     edge_color = flow,
                     include_source_sink = TRUE) +
  ggplot2::labs(caption="weighted by flow")

p_8am_slice_no_source_sink <-
  plot_small_network(G1_8am_slice,
                     num_accuracy = .01,
                     edge_label = rate_o,
                     include_source_sink = TRUE) +
  ggplot2::labs(caption="weighted by rate_o")


p_2communities <- plot_small_network(G_asympt, include_source_sink = TRUE)

p_2communities_no_source_sink <-
  plot_small_network(G_asympt, include_source_sink = FALSE)

p_multiple_small_plots <- G_asympt %>%
  small_network_plots(num_scale = 0.001, edge_label = flow, node_fill = NULL) %>%
  GGally::ggmatrix(nrow = 1, ncol = 2,
                   xAxisLabels = paste0("subgraph",c("1", "2"), sep = " "))
