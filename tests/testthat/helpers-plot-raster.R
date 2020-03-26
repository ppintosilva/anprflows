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

corridor_speed_rasters <- plot_spacetime_speed(corridor_flows)
corridor_flow_rasters <- plot_spacetime(corridor_flows, fill_var = flow)

corridor_flow_matrix <- GGally::ggmatrix(
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
