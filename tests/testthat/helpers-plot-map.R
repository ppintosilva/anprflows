# plot_map performs left join, colored locations, colored flows
p_map_asympt <-
  plot_map(
    spatial = spatial_1,
    flows = asympt_flows_od_1,
    aes_color_locations = "id",
    aes_color_flows = "flow")

# input with previously performed left join, colored flows
p_map_flows_no_arterial_no_color <-
  plot_map(
    spatial = spatial_1,
    flows = asympt_flows_od_1,
    aes_color_locations = "",
    aes_color_flows = "flow")

# input with previously performed left join, colored flows, add arterial
p_map_flows_arterial <-
  plot_map(
    spatial = spatial_1,
    flows = asympt_flows_od_1,
    aes_color_locations = "",
    aes_color_flows = "flow",
    add_arterial = TRUE)

# no need for flows, just plotting spatial data
p_map_no_flows_no_arterial <-
  plot_map(
    spatial_1,
    aes_color_locations = "id",
    locations_palette = "Set1")

# plot spatial data with arterial, remove locations and pairs
p_map_no_flows_no_paths <-
  plot_map(
    spatial_1,
    add_arterial = TRUE,
    add_locations = FALSE,
    add_paths = FALSE)

p_map_matrix <-
  plot_map_pairs(spatial_1, G1_asympt)
