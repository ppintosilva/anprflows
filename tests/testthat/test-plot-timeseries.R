incident <- readr::read_rds(
  system.file("testdata","congestion_20180316.rds",package="anprflows"))

test_that("timeseries demand plot works", {
  p_demand_l_05 <-
    plot_demand_l(
      incident$flows_05$l,
      time_breaks = incident$breaks
    )

  p_demand_l_15 <-
    plot_demand_l(
      incident$flows_15$l,
      time_breaks = incident$breaks,
      point_alpha = .3,
      point_size = 2,
    )

  p_demand_l_15 <-
    plot_demand_l(
      incident$flows_h$l,
      time_breaks = incident$breaks,
      title = "Hour flows at incident locations"
    )
})
