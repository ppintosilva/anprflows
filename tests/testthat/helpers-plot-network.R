# ---- Test different combinatoins of slice and raw/processed flow networks

p_asympt_no_source_sink_simple <-
  suppressMessages(
    plot_small_network(G1_asympt,
                       include_source_sink = FALSE,
                       aes_edge_label = "")
  )

p_asympt_no_source_sink <-
  suppressMessages(
    plot_small_network(G1_asympt,
                       num_scale = 0.001,
                       aes_edge_label = "flow",
                       include_source_sink = FALSE)
  )

p_asympt_no_spurious <-
  suppressMessages(
    plot_small_network(G1_asympt_no_spurious,
                       num_scale = 0.001,
                       aes_edge_label = "flow",
                       include_source_sink = TRUE)
  )

p_8am_slice <-
  suppressMessages(
    plot_small_network(G1_8am_slice,
                       num_accuracy = NULL,
                       aes_edge_label = "flow",
                       aes_edge_color = "flow",
                       include_source_sink = TRUE) +
    ggplot2::labs(caption="weighted by flow")
  )

p_8am_slice_no_source_sink <-
  suppressMessages(
    plot_small_network(G1_8am_slice,
                       num_accuracy = NULL,
                       aes_edge_label = "rate_o",
                       include_source_sink = TRUE) +
    ggplot2::labs(caption="weighted by rate_o")
  )


p_2communities <-
  suppressMessages(
    plot_small_network(G_asympt, include_source_sink = TRUE)
  )

p_2communities_no_source_sink <-
  suppressMessages(
    plot_small_network(G_asympt, include_source_sink = FALSE)
  )

p_multiple_small_plots <-
  suppressMessages(
    G_asympt %>%
    small_network_plots(num_scale = 0.001, aes_node_fill = "") %>%
    patchwork::wrap_plots(ncol = 2)
  )
