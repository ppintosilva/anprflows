time_breaks <- seq(
  lubridate::ymd_hms("2018-01-01 00:00:00"),
  lubridate::ymd_hms("2018-01-02 00:00:00"),
  by = "1 hour"
)

# Node demand plots

p_demand_l_0 <- plot_demand_l(flows_l_1)

p_demand_l_1 <- plot_demand_l(flows_l_1, include_source_sink = TRUE)

p_demand_l_2 <- plot_demand_l(
  flows_l_1,
  point_alpha = .3,
  point_size = 2,
  time_breaks = time_breaks,
  include_source_sink = TRUE,
  out_flow = FALSE
)

# OD demand plots

p_demand_od_1 <- plot_demand_od(flows_od_1)
p_demand_od_2 <- plot_demand_od(flows_od_1,
                                include_source_sink = TRUE,
                                time_breaks = time_breaks)


# OD speed plots

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
