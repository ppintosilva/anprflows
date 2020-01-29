context("plot-timeseries")

incident <- readr::read_rds(
  system.file("testdata","congestion_20180316.rds",package="anprflows"))

test_that("timeseries demand plot works", {
  p_demand_l_05 <-
    plot_demand_l(
      incident$flows_05$l,
      time_breaks = incident$breaks
    )

  vdiffr::expect_doppelganger("ggplot2 demand 5 min", p_demand_l_05)

  p_demand_l_15 <-
    plot_demand_l(
      incident$flows_15$l,
      time_breaks = incident$breaks,
      point_alpha = .3,
      point_size = 2,
    )

  vdiffr::expect_doppelganger("ggplot2 demand 15 min", p_demand_l_15)

  p_demand_l_h <-
    plot_demand_l(
      incident$flows_h$l,
      time_breaks = incident$breaks,
      title = "Hour flows at incident locations"
    )

  vdiffr::expect_doppelganger("ggplot2 demand hour", p_demand_l_h)
})
