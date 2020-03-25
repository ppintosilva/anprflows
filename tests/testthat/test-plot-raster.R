context("plot-raster")

skip_on_travis()

test_that("corridor_speed_rasters works", {
  vdiffr::expect_doppelganger("corridor_speed_rasters", corridor_speed_rasters)
})

test_that("corridor_flow_rasters works", {
  vdiffr::expect_doppelganger("corridor_flow_rasters", corridor_flow_rasters)
})
