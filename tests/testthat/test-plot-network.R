context("plot-network")

# Data ----

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

spatial1_filename <-
  system.file("testdata","spatial_1.rds", package="anprflows")

raw_flows <- read_flows_csv(filenames = test1_filename)

# summary of raw flows
summarised_raw_flows <-
  raw_flows %>%
  dplyr::group_by(o,d) %>%
  dplyr::summarise(flow = sum(flow))

# one time slice of raw flows
raw_flows_8am_slice <-
  raw_flows %>%
  dplyr::filter(lubridate::hour(t) == 8 & lubridate::minute(t) == 15)

# processed flows
flows_l <- get_flows_l(raw_flows)
flows_od <- get_flows_od(raw_flows, flows_l)

# one time slice of processed flows
flows_od_8am_slice <-
  flows_od %>%
  dplyr::filter(lubridate::hour(t) == 8 & lubridate::minute(t) == 15)


# Plots ----

p1a <- plot_small_network(summarised_raw_flows,
                          num_scale = 0.001,
                          ignore_source_sink = TRUE)

p1b <- plot_small_network(summarised_raw_flows,
                          num_accuracy = NULL,
                          aes_edge_label = "flow",
                          aes_edge_color = "flow")

p2a <- plot_small_network(raw_flows_8am_slice,
                          ignore_source_sink = TRUE)

p2b <- plot_small_network(raw_flows_8am_slice,
                          aes_edge_color = "flow")

p3a <- plot_small_network(flows_od_8am_slice,
                          aes_edge_color = "rate_o",
                          aes_edge_label = "rate_o",
                          num_accuracy = .01)

p3b <- plot_small_network(flows_od_8am_slice,
                          aes_edge_label = "rate_d",
                          num_accuracy = .01)

p3c <- plot_small_network(flows_od_8am_slice,
                          aes_edge_label = "",
                          aes_edge_color = "rate_d")

# Tests ----

test_that("plot graph stops if multiple time steps", {
  expect_error(
    plot_small_network(flows_od),
    regexp = "Input flow data must be summarised or windowed")
})

test_that("plot small network works", {
  vdiffr::expect_doppelganger(
    "small network 1 summarised a", p1a)
  vdiffr::expect_doppelganger(
    "small network 1 summarised b", p1b)
  vdiffr::expect_doppelganger(
    "small network 2 raw slice a", p2a)
  vdiffr::expect_doppelganger(
    "small network 2 raw slice b", p2b)
  vdiffr::expect_doppelganger(
    "small network 3 slice a", p3a)
  vdiffr::expect_doppelganger(
    "small network 3 slice b", p3b)
  vdiffr::expect_doppelganger(
    "small network 3 slice c",p3c)

  vdiffr::expect_doppelganger(
    "small network 2 raw slice a plus geom point",
    p2a +
      geom_node_point(aes(color = factor(name)), size = 4) +
      scale_color_brewer("name", palette = "Set2")
  )
})
