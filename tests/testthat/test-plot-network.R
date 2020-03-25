context("plot-network")

skip_on_travis()

test_that("plot graph stops if multiple time steps", {
  expect_error(
    plot_small_network(flows_od))
})

test_that("p_asympt_no_source_sink_simple works", {
  vdiffr::expect_doppelganger(
    "G1 asymptotic no source sink no labels", p_asympt_no_source_sink_simple)
})

test_that("p_asympt_no_source_sink works", {
  vdiffr::expect_doppelganger(
    "G1 asymptotic no source sink labels", p_asympt_no_source_sink)
})

test_that("p_asympt_no_spurious works", {
  vdiffr::expect_doppelganger(
    "G1 asymptotic no spurious", p_asympt_no_spurious)
})

test_that("p_8am_slice weighted flow works", {
  vdiffr::expect_doppelganger(
    "G1 8am slice weighted flow", p_8am_slice)
})

test_that("p_8am_slice_no_source_sink weighted rate_o works", {
  vdiffr::expect_doppelganger(
    "G1 8am slice weigthed rate_o", p_8am_slice_no_source_sink)
})

test_that("plot containing source sink + facet_nodes by subgraph fails ", {
  expect_error(lazyeval::f_eval(p_2communities + facet_nodes(~ subgraph)))
})

test_that("plot without source sink + facet_nodes by subgraph works", {
  vdiffr::expect_doppelganger("small network G with subgraph facet",
                              p_2communities_no_source_sink +
                                facet_nodes(~subgraph))
})

test_that("wrap plot of small networks", {
  vdiffr::expect_doppelganger("wrap of small network plots",
                              p_multiple_small_plots)
})
