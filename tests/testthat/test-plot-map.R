context("plot-map")

# Data ----

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

spatial1_filename <-
  system.file("testdata","spatial_1.rds", package="anprflows")


spatial_1 <- readr::read_rds(spatial1_filename)
raw_flows_1 <- read_flows_csv(filenames = test1_filename)

summarised_raw_flows <-
  raw_flows_1 %>%
  group_by(o,d) %>%
  summarise(flow = sum(flow))

summarised_spatial_flows_1 <-
  suppressWarnings(dplyr::left_join(spatial_1$pairs, raw_flows_1)) %>%
  group_by(o,d) %>%
  summarise(flow = sum(flow))

# Plots ----

# plot_map performs left join, colored locations, colored flows
p_1 <-
  plot_map(
    spatial = spatial_1,
    flows = summarised_raw_flows,
    aes_color_locations = "id",
    aes_color_flows = "flow")

# input with previously performed left join, colored flows
p_2 <-
  plot_map(
    spatial = spatial_1,
    flows = summarised_spatial_flows_1,
    aes_color_locations = "",
    aes_color_flows = "flow")

# input with previously performed left join, colored flows, add arterial
p_3 <-
  plot_map(
    spatial = spatial_1,
    flows = summarised_spatial_flows_1,
    aes_color_locations = "",
    aes_color_flows = "flow",
    add_arterial = TRUE)

# no need for flows, just plotting spatial data
p_4 <- plot_map(spatial_1,
                aes_color_locations = "id",
                locations_palette = "Set1")

# plot spatial data with arterial, remove locations and pairs
p_5 <- plot_map(spatial_1,
                add_arterial = TRUE,
                add_locations = FALSE,
                add_paths = FALSE)


# Tests ----

test_that("plot maps works", {
  vdiffr::expect_doppelganger("map with left join colors",p_1)
  vdiffr::expect_doppelganger("map not left join color flow", p_2)
  vdiffr::expect_doppelganger("map not left join color flow arterial", p_3)
  vdiffr::expect_doppelganger("no flows color locations palette", p_4)
  vdiffr::expect_doppelganger("no flows/color/locations/flows with arterial",p_5)
})
