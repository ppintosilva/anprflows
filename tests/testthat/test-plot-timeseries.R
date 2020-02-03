context("plot-timeseries")

incident <- readr::read_rds(
  system.file("testdata","congestion_20180316.rds",package="anprflows"))

test_that("timeseries demand location plot works", {
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
    )

  vdiffr::expect_doppelganger("ggplot2 demand hour", p_demand_l_h)
})

test_that("timeseries demand od plot works", {
  p_demand_od_05 <-
    plot_demand_od(
      incident$flows_05$od,
      time_breaks = incident$breaks
    )

  vdiffr::expect_doppelganger("ggplot2 demand od 5 min", p_demand_od_05)
})

test_that("timeseries speed od plot works", {
  p_speed_od_15 <-
    plot_speed_od(
      incident$flows_15$od,
      time_breaks = incident$breaks
    )

  vdiffr::expect_doppelganger("ggplot2 speed od 15 min", p_speed_od_15)

  p_speed_od_15_no_ribbon <-
    plot_speed_od(
      incident$flows_15$od,
      time_breaks = incident$breaks,
      add_ribbon = FALSE
    )

  vdiffr::expect_doppelganger("ggplot2 speed od 15 min no ribbon",
                              p_speed_od_15_no_ribbon)
})
