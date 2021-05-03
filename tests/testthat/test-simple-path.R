# Flow graph

G_edges <-
  asympt_flows_od %>%
  filter(o != "SOURCE") %>%
  filter(d != "SINK") %>%
  select(o,d)

G <- igraph::graph_from_data_frame(G_edges)


test_that("multiplication works", {
  testthat::expect_true(
    is_simple_path(G, c("77","209","54"))
  )

  testthat::expect_true(
    is_simple_path(G, c("133","199"))
  )

  testthat::expect_false(
    is_simple_path(G, c("133","77"))
  )

  testthat::expect_false(
    is_simple_path(G, c("77","209","133"))
  )

})
