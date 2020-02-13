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

test_that("get neighbors from network works", {

  subnetwork209 <-
    get_neighbors(G, "209")

  expect_equal(
    subnetwork209 %>%
      activate(nodes) %>%
      as_tibble() %>%
      pull(name) %>%
      as.character(),
    c("54", "77", "209", "SOURCE")
  )

  expect_equal(
    subnetwork209 %>%
      activate(edges) %>%
      as_tibble() %>%
      select(from, to),
    tibble("from" = as.integer(c(2,3,4,4)),
           "to" = as.integer(c(3,1,2,3)))
  )

  subnetwork54 <-
    get_neighbors(G, "54")

  expect_equal(
    subnetwork54 %>%
      activate(nodes) %>%
      as_tibble() %>%
      pull(name) %>%
      as.character(),
    c("54", "209", "SINK")
  )

  subnetwork_209_54 <-
    get_neighbors(G, c("209", "54"))

  expect_equal(
    subnetwork_209_54 %>% activate("edges") %>% as_tibble() %>% select(from,to),
    G1 %>% activate("edges") %>% as_tibble() %>% select(from,to)
  )
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

test_that("get neighbors from network works", {

  subnetwork209 <-
    get_neighbors(G, "209")

  expect_warning(get_neighbors(G, c("209", "10000")),
                 regexp = "Ignoring nodes")
})

test_that("get subgraphs works", {

  subG <- get_subgraphs(G, include_source_sink = TRUE)

  expected_subG1 <-
    G %>% activate("nodes") %>% filter(subgraph == 1 | is.na(subgraph))

  expected_subG2 <-
    G %>% activate("nodes") %>% filter(subgraph == 2 | is.na(subgraph))

  expect_equal(subG[[1]] %>% as_tibble,
               expected_subG1 %>% as_tibble)
  expect_equal(subG[[2]] %>% as_tibble,
               expected_subG2 %>% as_tibble)

  expect_equal(subG[[1]] %>% activate("edges") %>% as_tibble,
               expected_subG1 %>% activate("edges") %>% as_tibble)
  expect_equal(subG[[2]] %>% activate("edges") %>% as_tibble,
               expected_subG2 %>% activate("edges") %>% as_tibble)

  subG_no_s <- get_subgraphs(G, include_source_sink = FALSE)

  expected_subG1_no_s <-
    G %>% activate("nodes") %>% filter(subgraph == 1)

  expected_subG2_no_s <-
    G %>% activate("nodes") %>% filter(subgraph == 2)

  expect_equal(subG_no_s[[1]] %>% as_tibble,
               expected_subG1_no_s %>% as_tibble)
  expect_equal(subG_no_s[[2]] %>% as_tibble,
               expected_subG2_no_s %>% as_tibble)

  expect_equal(subG_no_s[[1]] %>% activate("edges") %>% as_tibble,
               expected_subG1_no_s %>% activate("edges") %>% as_tibble)
  expect_equal(subG_no_s[[2]] %>% activate("edges") %>% as_tibble,
               expected_subG2_no_s %>% activate("edges") %>% as_tibble)
})
