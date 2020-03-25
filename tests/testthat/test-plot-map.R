context("plot-map")

skip_on_travis()

test_that("p_map_asympt works", {
  vdiffr::expect_doppelganger("p_map_asympt", p_map_asympt)
})

test_that("p_map_flows_no_arterial_no_color works", {
  vdiffr::expect_doppelganger("p_map_flows_no_arterial_no_color",
                              p_map_flows_no_arterial_no_color)
})

test_that("p_map_flows_arterial works", {
  vdiffr::expect_doppelganger("p_map_flows_arterial",
                              p_map_flows_arterial)
})

test_that("p_map_no_flows_no_arterial works", {
  vdiffr::expect_doppelganger("p_map_no_flows_no_arterial",
                              p_map_no_flows_no_arterial)
})

test_that("p_map_no_flows_no_paths works", {
  vdiffr::expect_doppelganger("p_map_no_flows_no_paths",
                              p_map_no_flows_no_paths)
})

test_that("plot_map_pairs works", {
  vdiffr::expect_doppelganger("p_map_matrix",
                              p_map_matrix)
})
