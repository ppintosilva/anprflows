test_that("factor levels before and after are the same", {
  # test that flow network factors remain the same
  levels_before <- levels(asympt_flows_od$o)
  levels_after <- levels(
    G_asympt %>% activate(nodes) %>% as_tibble %>% pull(name)
  )

  expect_equal(levels_before, levels_after)
})

test_that("flow network works", {

  observed_nrow <-
    G1_asympt %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    nrow()

  # two edges are removed
  expected_nrow <- 6

  expect_true(observed_nrow == expected_nrow)
})

test_that("flow network with two subgraphs is labelled correctly", {

  subgraphs <-
    G_asympt %>%
    activate(nodes) %>%
    as_tibble() %>%
    distinct(subgraph) %>%
    pull(subgraph)

  expect_equal(subgraphs, c(1,2,NA))
})

test_that("get neighbors from network works", {

  subnetwork209 <-
    get_neighbors(G_asympt, "209")

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
    get_neighbors(G_asympt, "54")

  expect_equal(
    subnetwork54 %>%
      activate(nodes) %>%
      as_tibble() %>%
      pull(name) %>%
      as.character(),
    c("54", "209", "SINK")
  )

  subnetwork_209_54 <-
    get_neighbors(G_asympt, c("209", "54"))

  expect_equal(
    subnetwork_209_54 %>% activate("edges") %>% as_tibble() %>% select(from,to),
    G1_asympt %>% activate("edges") %>% as_tibble() %>% select(from,to)
  )
})

test_that("flow network with two subgraphs is labelled correctly", {

  subgraphs <-
    G_asympt %>%
    activate(nodes) %>%
    as_tibble() %>%
    distinct(subgraph) %>%
    pull(subgraph)

  expect_equal(subgraphs, c(1,2,NA))
})

test_that("get neighbors works with inexistent node with warning", {

  subnetwork209 <-
    get_neighbors(G_asympt, "209")

  expect_warning(get_neighbors(G_asympt, c("209", "10000")),
                 regexp = "Ignoring nodes")
})

test_that("get subgraphs works", {

  subG <- get_subgraphs(G_asympt, include_source_sink = TRUE)

  expected_subG1 <-
    G_asympt %>% activate("nodes") %>% filter(subgraph == 1 | is.na(subgraph))

  expected_subG2 <-
    G_asympt %>% activate("nodes") %>% filter(subgraph == 2 | is.na(subgraph))

  expect_equal(subG[[1]] %>% as_tibble,
               expected_subG1 %>% as_tibble)
  expect_equal(subG[[2]] %>% as_tibble,
               expected_subG2 %>% as_tibble)

  expect_equal(subG[[1]] %>% activate("edges") %>% as_tibble,
               expected_subG1 %>% activate("edges") %>% as_tibble)
  expect_equal(subG[[2]] %>% activate("edges") %>% as_tibble,
               expected_subG2 %>% activate("edges") %>% as_tibble)

  subG_no_s <- get_subgraphs(G_asympt, include_source_sink = FALSE)

  expected_subG1_no_s <-
    G_asympt %>% activate("nodes") %>% filter(subgraph == 1)

  expected_subG2_no_s <-
    G_asympt %>% activate("nodes") %>% filter(subgraph == 2)

  expect_equal(subG_no_s[[1]] %>% as_tibble,
               expected_subG1_no_s %>% as_tibble)
  expect_equal(subG_no_s[[2]] %>% as_tibble,
               expected_subG2_no_s %>% as_tibble)

  expect_equal(subG_no_s[[1]] %>% activate("edges") %>% as_tibble,
               expected_subG1_no_s %>% activate("edges") %>% as_tibble)
  expect_equal(subG_no_s[[2]] %>% activate("edges") %>% as_tibble,
               expected_subG2_no_s %>% activate("edges") %>% as_tibble)
})
