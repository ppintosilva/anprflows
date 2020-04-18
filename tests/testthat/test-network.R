# tests ----

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
    mutate(subgraph = forcats::fct_explicit_na(subgraph, na_level = "NA")) %>%
    pull(subgraph)

  expect_equal(levels(subgraphs), c("1","2","NA"))
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
    mutate(subgraph = forcats::fct_explicit_na(subgraph, na_level = "NA")) %>%
    pull(subgraph)

  expect_equal(levels(subgraphs), c("1","2","NA"))
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

test_that("all paths works, no source sink", {

  subgraphs <- get_subgraphs(G_asympt, include_source_sink = FALSE)

  paths <- vec_all_paths(subgraphs)

  observed_path_stats <- paths %>%
    group_by(subgraph, path) %>%
    summarise(N = dplyr::n())

  expected_path_stats <-
    tribble(
      ~subgraph, ~path, ~N,
      #---,---,
      1, 1, 3, # 77 -> 209 -> 54
      2, 1, 3  # 133 -> 112 -> 199
    ) %>%
    mutate(subgraph = as.character(subgraph)) %>%
    mutate_if(is.double, as.integer)

  expect_equal(expected_path_stats, observed_path_stats)
})

test_that("all paths works, 2ith source sink", {

  subgraphs <- get_subgraphs(G_asympt, include_source_sink = TRUE)

  paths <- vec_all_paths(subgraphs)

  observed_path_stats <- paths %>%
    group_by(subgraph, path) %>%
    summarise(N = dplyr::n()) %>%
    arrange(subgraph, N) %>%
    select(subgraph, N)

  expected_path_stats <- tribble(
    ~subgraph, ~N,
    #---,---,
    # subgraph 1: 5 nodes, 6  edges
    # (edges SOURCE -> 54 and 209 -> SINK have been remove as spurious)
    1, 3, # SOURCE -> 77 -> SINK
    1, 4, # SOURCE -> 77 -> 209 -> SINK
    1, 5, # SOURCE -> 77 -> 209 -> 54 -> SINK
    # subgraph 2: 5 nodes, 7  edges
    # (edges SOURCE -> 54 and 209 -> SINK have been remove as spurious)
    2, 3, # SOURCE -> 133 -> SINK
    2, 3, # SOURCE -> 112 -> SINK
    2, 4, # SOURCE -> 133 -> 112 -> SINK
    2, 4, # SOURCE -> 112 -> 199 -> SINK
    2, 5, # SOURCE -> 133 -> 112 -> 199 -> SINK
  ) %>%
    mutate(subgraph = as.character(subgraph),
           N = as.integer(N))

  expect_equal(expected_path_stats, observed_path_stats)
})

test_that("flow network with 0 nodes returns empty tbl_graph", {

  G1 <- flow_network(
    asympt_flows_od_1,
    label_subgraphs = FALSE,
    spurious_if_below = c("rate_o" = 1.0),
    keep_levels = TRUE
  )

  G2 <- flow_network(
    asympt_flows_od_1,
    label_subgraphs = TRUE,
    spurious_if_below = c("rate_o" = 1.0),
    keep_levels = TRUE
  )

  expect_equal(igraph::gsize(G1), 0)
  expect_equal(igraph::gsize(G2), 0)
})
