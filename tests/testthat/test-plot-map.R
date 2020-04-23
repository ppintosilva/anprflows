context("plot-map")

skip_on_travis()

# test data ----

# no need for flows, just plotting spatial data
p_map_default <- plot_map(spatial_1)

# plot spatial data with arterial, remove locations and pairs
p_map_raw <-
  plot_map(
    spatial_1,
    add_arterial = TRUE,
    add_locations = FALSE,
    add_paths = FALSE)

# plot_map performs left join, coloured flows
p_map_asympt <-
  plot_map(
    spatial = spatial_1,
    network = G1_asympt,
    aes_color_flows = "flow",
    add_arterial = TRUE)

p_map_matrix <-
  plot_map_matrix(spatial_1, G1_asympt)


# tests ----

test_that("p_map_default", {
  vdiffr::expect_doppelganger("p_map_default", p_map_default)
})

test_that("p_map_asympt works", {
  vdiffr::expect_doppelganger("p_map_asympt", p_map_asympt)
})

test_that("p_map_raw works", {
  vdiffr::expect_doppelganger("p_map_raw", p_map_raw)
})

test_that("plot_map_pairs works", {
  vdiffr::expect_doppelganger("p_map_matrix", p_map_matrix)
})
