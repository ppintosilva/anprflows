context("plot-network")

# Data ----

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

spatial1_filename <-
  system.file("testdata","spatial_1.rds", package="anprflows")

raw_flows <- read_flows_csv(filenames = test1_filename)

no_spurious <- NULL

# summary of raw flows
summarised_raw_flows <-
  raw_flows %>%
  dplyr::group_by(o,d) %>%
  dplyr::summarise(flow = sum(flow))

network_summarised_raw <-
  flow_network(flows = summarised_raw_flows,
               spurious_if_below = no_spurious)


# one time slice of raw flows
raw_flows_8am_slice <-
  raw_flows %>%
  dplyr::filter(lubridate::hour(t) == 8 & lubridate::minute(t) == 15)

network_raw_8am <-
  flow_network(flows = raw_flows_8am_slice,
               spurious_if_below = no_spurious)

# processed flows
flows_l <- get_flows_l(raw_flows)
flows_od <- get_flows_od(raw_flows, flows_l)

# one time slice of processed flows
flows_od_8am_slice <-
  flows_od %>%
  dplyr::filter(lubridate::hour(t) == 8 & lubridate::minute(t) == 15)

network_processed_8am <-
  flow_network(flows = flows_od_8am_slice,
               spurious_if_below = no_spurious)

# Plots ----

p1a <- plot_small_network(network_summarised_raw,
                          num_scale = 0.001,
                          include_source_sink = FALSE)

p1b <- plot_small_network(network_summarised_raw,
                          num_accuracy = NULL,
                          aes_edge_label = "flow",
                          aes_edge_color = "flow")

p2a <- plot_small_network(network_raw_8am,
                          include_source_sink = FALSE)

p2b <- plot_small_network(network_raw_8am,
                          aes_edge_color = "flow")

p3a <- plot_small_network(network_processed_8am,
                          aes_edge_color = "rate_o",
                          aes_edge_label = "rate_o",
                          num_accuracy = .01)

p3b <- plot_small_network(network_processed_8am,
                          aes_edge_label = "rate_d",
                          num_accuracy = .01)

p3c <- plot_small_network(network_processed_8am,
                          aes_edge_label = "",
                          aes_edge_color = "rate_d")

# Tests 1 ----

test_that("plot graph stops if multiple time steps", {
  expect_error(
    plot_small_network(flows_od))
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
    "small network 3 slice c", p3c)
})

# Tests 2 ----

test2_filename <-
  system.file("testdata","small_flows_dataset_set2.csv", package="anprflows")

# Both flows together
raw_flows   <-
  read_flows_csv(filenames = c(test1_filename,test2_filename)) %>%
  dplyr::arrange(o,d,t)

asympt_flows_l <- get_flows_l(raw_flows,
                              by_period = FALSE)

asympt_flows_od <- get_flows_od(raw_flows, asympt_flows_l,
                                by_period = FALSE)

G <- flow_network(asympt_flows_od,
                  label_subgraphs = TRUE,
                  spurious_if_below = c("rate_o" = .10),
                  names_as_factors = FALSE)

test_that("plot small network + facet_node works", {
  pl <- plot_small_network(G,
                           num_scale = 0.001,
                           include_source_sink = TRUE)

  vdiffr::expect_doppelganger("small network G with source sink", pl)

  expect_error(lazyeval::f_eval(pl + facet_nodes(~ subgraph)))

  pl <- plot_small_network(G,
                           num_scale = 0.001,
                           include_source_sink = FALSE)

  vdiffr::expect_doppelganger("small network G without source sink", pl)

  pl_facet <- pl + facet_nodes(~ subgraph)


  vdiffr::expect_doppelganger("small network G with subgraph facet", pl_facet)
})

test_that("wrap plot of small networks", {

  pl <-
    G %>%
    small_network_plots(num_scale = 0.001, aes_node_fill = "") %>%
    patchwork::wrap_plots(ncol = 2)

  vdiffr::expect_doppelganger("wrap of small network plots", pl)
})
