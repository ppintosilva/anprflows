# determine ordinary trips from trip data and flow graph

shortest_path <- function(G, s, t) {

  vs <-
    G %>%
    tidygraph::as.igraph() %>%
    igraph::shortest_paths(from = s, to = t, weights = igraph::edge_attr(., "dt")) %>%
    purrr::pluck("vpath") %>%
    purrr::pluck(1) %>%
    as.integer()

  es <- tibble(
    from = vs,
    to = lead(vs)
  ) %>%
    filter(!is.na(to)) %>%
    mutate(step = row_number())

  G %>%
    activate(edges) %>%
    inner_join(es, by = c("from", "to")) %>%
    arrange(step) %>%
    as_tibble()
}

toy_road <- readr::read_rds(system.file("testdata","toy_road.rds", package="anprflows"))
# toy_road <- example_roadnet()

toy_road_edges <- toy_road %>% activate(edges) %>% as_tibble()

toy_road_nodes <- toy_road %>% activate(nodes) %>% as_tibble() %>% select(name, id)

flow_graph_edges <- tribble(
  ~from, ~to,
  "A", "B",
  "B", "C",
  "B", "D",
  "B", "E",
  "D", "E",
  "D", "F",
  "F", "G",
  "B", "F",
  "D", "G"
) %>%
  mutate(route = row_number()) %>%
  unite("route_label", from, to , sep = " -> ", remove = F) %>%
  select(from, to, route, route_label)

distances <-
  tibble(
    from = LETTERS[1:7],
    to = LETTERS[1:7]
  ) %>%
  tidyr::expand(from, to) %>%
  filter(from != to) %>%
  inner_join(toy_road_nodes, by = c("from" = "name")) %>%
  inner_join(toy_road_nodes, by = c("to" = "name"), suffix = c("1", "2")) %>%
  mutate(
    distance = purrr::map2_dbl(id1, id2, .f = ~ {
      shortest_path(toy_road, .x, .y) %>%
        summarise(d = sum(d)) %>%
        pull(d)
    }
  )) %>%
  rename(o = from, d = to) %>%
  select(o,d,distance)


# build corridor set

expected_corridors <- tibble::tribble(
  ~corridor,  ~o,  ~d,
  1L, "B", "D",
  1L, "B", "F",
  1L, "D", "F",
  1L, "F", "G",
  1L, "D", "G",
  2L, "A", "B",
  2L, "B", "C",
  3L, "A", "B",
  3L, "B", "E",
  4L, "D", "E"
)


Gtoy <- igraph::graph_from_data_frame(flow_graph_edges %>% select(from,to))

observed_days <- 10L

observed_sequences <- tibble::tribble(
  ~s, ~n,
  # corridor 1 and 2
  "A,B", 100L,
  "B,C", 100L,
  "A,B,C", 100L,
  "A,B,E", 100L,
  # corridor 4
  "D,E", 100L,
  # corridor 3
  "B,D", 100L,
  "B,D,F", 50L,
  "B,F", 50L,
  "B,F,G", 100L,
  "B,D,F,G", 100L,
  "D,G", 100L,
  # bad ones
  "D,E,G", 10L,
  "B,E,G", 10L,
  "G,E", 10L,
  "D,C,B", 10L
)

toy_ordinary_sequences <-
  observed_sequences %>%
  expand_sequences() %>%
  reduce_sequences() %>%
  route_utility(distances, .keep_distances = F) %>%
  mutate(rate = n/observed_days) %>%
  ordinary_sequences(
    G = Gtoy,
    min_rate = 5,
    min_utility = 0.75
  )

toy_corridors <- get_corridor_set(toy_ordinary_sequences)


test_that("toy corridors are computed as expected", {

  expect_equal(toy_corridors %>% arrange(corridor,o,d),
               expected_corridors %>% arrange(corridor,o,d))

})

test_that("corridors wrapper works", {
  computed_corridors <- get_corridors(
    observed_sequences,
    distances = distances,
    G = Gtoy,
    ndays = observed_days,
    min_rate = 5,
    min_utility = 0.75
  )

  expect_equal(computed_corridors %>% arrange(corridor,o,d),
               expected_corridors %>% arrange(corridor,o,d))
})
