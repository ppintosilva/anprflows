test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

test2_filename <-
  system.file("testdata","small_flows_dataset_set2.csv", package="anprflows")

raw_flows_1 <- read_flows_csv(filenames = test1_filename)

raw_flows_2 <- read_flows_csv(filenames = test2_filename)

asympt_flows_1_l <- get_flows_l(raw_flows_1,
                                by_period = FALSE)

asympt_flows_1_od <- get_flows_od(raw_flows_1, asympt_flows_1_l,
                                  by_period = FALSE)

# default thresholds = 0.1, 0.1
G1 <- flow_network(asympt_flows_1_od)

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
                  spurious_if_below = c("rate_o" = .10))

test_that("flow network works", {

  observed_nrow <-
    G1 %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    nrow()

  # two edges are removed
  expected_nrow <- 6

  expect_true(observed_nrow == expected_nrow)
})

test_that("flow network with two subgraphs is labelled correctly", {

  subgraphs <-
    G %>%
    activate(nodes) %>%
    as_tibble() %>%
    distinct(subgraph) %>%
    pull(subgraph)

  expect_equal(subgraphs, c(1,2,NA))
})
