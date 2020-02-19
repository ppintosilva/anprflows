context("plot-timeseries")

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
