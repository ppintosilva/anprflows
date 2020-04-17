context("plot-raster")

skip_on_travis()

# test data ----

corridor_flows <-
  flows_od %>%
  cut_flows(
    flows_l,
    time_resolution = "15 min",
    filter_dayperiod = NULL,
    pairs = tibble(o = c("77","209"), d = c("209","54")),
    fill_gaps = TRUE
  ) %>% .$od %>%
  mutate(
    o = factor(o, levels =  c("77","209","54")),
    d = factor(d, levels =  c("77","209","54"))
  )

corridor_speed_rasters <-
  plot_spacetime(
    corridor_flows,
    fill_var = mean_speed
  ) %>%
  scale_fill_speed()

corridor_flow_rasters <- plot_spacetime(corridor_flows, fill_var = flow)

corridor_flow_matrix <-
  GGally::ggmatrix(
    plots = spacetime_plotlist(
      corridor_flows,
      fill_var = flow,
      facet_by = lubridate::hour(t),
      date_breaks = "15 min",
      date_labels = "%Mm"
    ),
    nrow = 4, ncol = 6,
    xAxisLabels = paste0(c("00", "01", "02", "03", "04", "05"), "h"),
    yAxisLabels = paste0(c("+00", "+06", "+12", "+18"), "h"),
    legend = c(1,1)
  )

corridor_speed_matrix <-
  GGally::ggmatrix(
    plots = spacetime_plotlist(
      corridor_flows,
      fill_var = mean_speed,
      facet_by = lubridate::hour(t),
      date_breaks = "15 min",
      date_labels = "%Mm"
    ) %>% lapply(., scale_fill_speed, limits = c(0,75)),
    nrow = 4, ncol = 6,
    xAxisLabels = paste0(c("00", "01", "02", "03", "04", "05"), "h"),
    yAxisLabels = paste0(c("+00", "+06", "+12", "+18"), "h"),
    legend = c(1,1)
  )


# tests ----

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
