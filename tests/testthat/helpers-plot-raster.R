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

corridor_speed_rasters <- plot_corridor_time_speed(corridor_flows)
corridor_flow_rasters <- plot_corridor_time(corridor_flows,
                                            fill_var = flow)
