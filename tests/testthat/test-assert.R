test_that("assert tibble works", {
  expect_true(assert_tibble(raw_flows))
  expect_error(assert_tibble(G_asympt))
})

test_that("assert cols works", {
  expect_true(assert_cols(raw_flows, c("o", "d", "t")))
  expect_error(assert_cols(raw_flows, c("o", "d", "density")))
})

test_that("assert tidygraph works", {
  expect_true(assert_tidygraph(G_asympt))
  expect_error(assert_tidygraph(raw_flows))
})
