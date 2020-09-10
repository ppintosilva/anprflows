test_sequences <- tibble::tribble(
  ~s, ~n,
  "1,2,3,4,5", 100L, # seq 1
  "1,2", 1000L, # seq 2
  "2,3", 2000L, # seq 3
  "3,4,5", 400L, # seq 4
  "A,B,C,D", 200L # seq 5
)

expanded_sequences <- expand_sequences(test_sequences) %>%
  arrange(s,n)

reduced_sequences <- expanded_sequences %>%
  reduce_sequences() %>%
  arrange(s,n)

test_that("sequences tibble is expanded correctly", {
  expected_s <- c(
    # expanded seq 1
    "1,2", "2,3", "3,4", "4,5", "1,2,3", "2,3,4", "3,4,5",
    "1,2,3,4", "2,3,4,5", "1,2,3,4,5",
    # expanded seq 2
    "1,2",
    # expanded seq 3,
    "2,3",
    # expanded seq 4
    "3,4", "4,5", "3,4,5",
    # expanded seq 5
    "A,B", "B,C", "C,D", "A,B,C", "B,C,D", "A,B,C,D"
  )

  # test that n column has not been changed
  expected_n <- c(
    rep(100L, 10), 1000L, 2000L, rep(400L,3), rep(200L,6)
  )

  expected_tibble <- tibble(
    s = expected_s,
    n = expected_n
  ) %>%
    mutate(l = stringr::str_count(s, ",") + 1) %>%
    arrange(s,n)

  expect_equal(expanded_sequences, expected_tibble)
})

test_that("sequences tibble is reduced correctly", {
  expected_s <- c(
    # new value for these
    "1,2", "2,3", "3,4", "4,5", "3,4,5",
    # original value
    "1,2,3", "2,3,4", "1,2,3,4", "2,3,4,5", "1,2,3,4,5",
    # original value
    "A,B", "B,C", "C,D", "A,B,C", "B,C,D", "A,B,C,D"
  )

  # test that n column has not been changed
  expected_n <- c(
    1100L, 2100L, 500L, 500L, 500L,
    rep(100L, 5),  rep(200L,6)
  )

  expected_tibble <- tibble(
    s = expected_s,
    n = expected_n
  ) %>%
    mutate(l = stringr::str_count(s, ",") + 1) %>%
    arrange(s,n)

  expect_equal(reduced_sequences, expected_tibble)
})
