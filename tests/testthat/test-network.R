test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

raw_flows_1 <- read_flows_csv(filenames = test1_filename)

asympt_flows_1_l <- get_flows_l(raw_flows_1,
                                by_period = FALSE)

asympt_flows_1_od <- get_flows_od(raw_flows_1, asympt_flows_1_l,
                                  by_period = FALSE)


test_that("flow network works", {

  # thresholds = 0.1, 0.1
  G <- flow_network(asympt_flows_1_od)

  observed_nrow <-
    G %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    nrow()

  # two edges are removed
  expected_nrow <- 6

  expect_true(observed_nrow == expected_nrow)
})
