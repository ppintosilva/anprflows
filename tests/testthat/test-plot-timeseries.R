context("plot-timeseries")

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

raw_flows <- read_flows_csv(filenames = test1_filename)

flows_l <- get_flows_l(raw_flows)
flows_od <- get_flows_od(raw_flows, flows_l)

time_breaks <- seq(
  lubridate::ymd_hms("2018-01-01 00:00:00"),
  lubridate::ymd_hms("2018-01-02 00:00:00"),
  by = "1 hour"
)

test_that("timeseries demand location plot works", {
  p_demand_l_0 <- plot_demand_l(flows_l)

  p_demand_l_1 <- plot_demand_l(flows_l, include_source_sink = TRUE)

  p_demand_l_2 <- plot_demand_l(
    flows_l,
    point_alpha = .3,
    point_size = 2,
    time_breaks = time_breaks,
    include_source_sink = TRUE,
    out_flow = FALSE
  )

  vdiffr::expect_doppelganger("ggplot2 flows_l no source sink", p_demand_l_0)
  vdiffr::expect_doppelganger("ggplot2 flows_l ", p_demand_l_1)
  vdiffr::expect_doppelganger("ggplot2 flows_l with params", p_demand_l_2)
})

test_that("timeseries demand od plot works", {
  p_demand_od_1 <- plot_demand_od(flows_od)
  p_demand_od_2 <- plot_demand_od(flows_od,
                                  include_source_sink = TRUE,
                                  time_breaks = time_breaks)

  vdiffr::expect_doppelganger("ggplot2 demand od", p_demand_od_1)
  vdiffr::expect_doppelganger("ggplot2 demand od with breaks", p_demand_od_2)
})

test_that("timeseries speed od plot works", {
  p_speed_od <- plot_speed_od(flows_od)

  p_speed_od_no_ribbon <-
    plot_speed_od(
      flows_od,
      time_breaks = time_breaks,
      add_ribbon = FALSE
    )

  vdiffr::expect_doppelganger("ggplot2 speed od", p_speed_od)
  vdiffr::expect_doppelganger("ggplot2 speed od no ribbon",
                              p_speed_od_no_ribbon)
})
