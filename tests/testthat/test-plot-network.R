context("plot-network")

skip_on_travis()

# test data ----

# test data ----

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

# tests ----

test_that("plot graph stops if multiple time steps", {
  expect_error(plot_small_network(flows_od))
})

test_that("p_asympt_no_source_sink_simple works", {
  vdiffr::expect_doppelganger(
    "G1 asymptotic no source sink no labels",
    p_asympt_no_source_sink_simple
  )
})

test_that("p_asympt_no_source_sink works", {
  vdiffr::expect_doppelganger(
    "G1 asymptotic no source sink labels",
    p_asympt_no_source_sink
  )
})

test_that("p_asympt_no_spurious works", {
  vdiffr::expect_doppelganger(
    "G1 asymptotic no spurious",
    p_asympt_no_spurious
  )
})

test_that("p_8am_slice weighted flow works", {
  vdiffr::expect_doppelganger(
    "G1 8am slice weighted flow",
    p_8am_slice
  )
})

test_that("p_8am_slice_no_source_sink weighted rate_o works", {
  vdiffr::expect_doppelganger(
    "G1 8am slice weigthed rate_o",
    p_8am_slice_no_source_sink
  )
})

test_that("plot containing source sink + facet_nodes by subgraph fails ", {
  expect_error(
    lazyeval::f_eval(p_2communities + ggraph::facet_nodes(~ subgraph))
  )
})

test_that("plot without source sink + facet_nodes by subgraph works", {
  vdiffr::expect_doppelganger(
    "small network G with subgraph facet",
    p_2communities_no_source_sink + ggraph::facet_nodes(~subgraph)
  )
})

test_that("wrap plot of small networks", {
  vdiffr::expect_doppelganger(
    "wrap of small network plots",
    p_multiple_small_plots
  )
})
