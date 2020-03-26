context("plot-raster")

skip_on_travis()

test_that("corridor_speed_rasters works", {
  vdiffr::expect_doppelganger("corridor_speed_rasters", corridor_speed_rasters)
})

test_that("corridor_flow_rasters works", {
  vdiffr::expect_doppelganger("corridor_flow_rasters", corridor_flow_rasters)
})

test_that("corridor_flow_list into matrix works", {
  vdiffr::expect_doppelganger("corridor_flow_matrix", corridor_flow_matrix)
})

test_that("corridor_speed_list into matrix works", {
  vdiffr::expect_doppelganger("corridor_speed_matrix", corridor_speed_matrix)
})
