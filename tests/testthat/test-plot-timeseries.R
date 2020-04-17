context("plot-timeseries")

skip_on_travis()

# test data ----

time_breaks <- seq(
  lubridate::ymd_hms("2018-01-01 00:00:00"),
  lubridate::ymd_hms("2018-01-02 00:00:00"),
  by = "1 hour"
)

## Node demand plots

p_demand_l_0 <- plot_demand_l(flows_l_1)

p_demand_l_1 <- plot_demand_l(flows_l_1, include_source_sink = TRUE)

p_demand_l_2 <- plot_demand_l(
  flows_l_1,
  point_alpha = .3,
  point_size = 2,
  time_breaks = time_breaks,
  full_datetime_format = "%y-%m-%d %Hh",
  partial_datetime_format = "%Hh",
  include_source_sink = TRUE,
  out_flow = FALSE
)

## OD demand plots

p_demand_od_1 <- plot_demand_od(flows_od_1)
p_demand_od_2 <- plot_demand_od(flows_od_1,
                                include_source_sink = TRUE,
                                time_breaks = time_breaks)


## OD speed plots

p_speed_od <- plot_speed_od(flows_od_1)

p_speed_od_no_color <-
  plot_speed_od(
    flows_od_1 %>% filter(o == 209 & d == 54),
    aes_color = "")

p_speed_od_no_ribbon <-
  plot_speed_od(
    flows_od_1,
    time_breaks = time_breaks,
    add_ribbon = FALSE
  )

## OD matrix plots

p_demand_matrix <- plot_ts_matrix(flows_od_1, type = "demand")
p_speed_matrix <- plot_ts_matrix(flows_od_1, type = "speed")


# tests ----

test_that("timeseries demand location plots work", {
  vdiffr::expect_doppelganger("plot flows_l out no source", p_demand_l_0)
  vdiffr::expect_doppelganger("plot flows_l out", p_demand_l_1)
  vdiffr::expect_doppelganger("plot flows_l in with params", p_demand_l_2)
})

test_that("timeseries demand od plots work", {
  vdiffr::expect_doppelganger("plot demand od no source sink", p_demand_od_1)
  vdiffr::expect_doppelganger("plot demand od with breaks", p_demand_od_2)
})

test_that("timeseries speed od plots work", {
  vdiffr::expect_doppelganger("plot speed od",
                              p_speed_od)
  vdiffr::expect_doppelganger("plot speed single od no color",
                              p_speed_od_no_color)
  vdiffr::expect_doppelganger("plot speed od no ribbon",
                              p_speed_od_no_ribbon)
})

test_that("timeseries matrix plot work", {
  vdiffr::expect_doppelganger("plot ts matrix demand", p_demand_matrix)
  vdiffr::expect_doppelganger("plot ts matrix speed", p_speed_matrix)
})
