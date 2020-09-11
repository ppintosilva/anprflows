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
    mutate(l = stringr::str_count(s, ",") + 1L) %>%
    arrange(s,n)

  expect_equal(reduced_sequences, expected_tibble)
})

test_that("sequences are joined correct (method exhaustive)", {
  expected_tibble <- tribble(
    ~s, ~l, ~n,
    "112,199", 2L, 500L,
    "77,209", 2L, 200L + 400L,
    "209,54", 2L, 200L + 400L,
    "133,112", 2L, 500L,
    "77,209,54", 3L, 200L,
    "133,112,199", 3L, 0L
  ) %>%
    arrange(s)

  expect_equal(mock_joined_sequences, expected_tibble)
})

test_that("utility loss works correctly", {
  observed_loss_l2 <-
    mock_pre_ordinary_sequences %>%
    filter(l == 2) %>%
    pull(uloss)

  observed_loss_77_209_54 <-
    mock_pre_ordinary_sequences %>%
    filter(s == "77,209,54") %>%
    pull(uloss)

  observed_loss_133_112_199 <-
    mock_pre_ordinary_sequences %>%
    filter(s == "133,112,199") %>%
    pull(uloss)

  purrr::walk(observed_loss_l2, ~ expect_lt(.x, 1e-06))
  expect_lt(abs(observed_loss_77_209_54 - (1-4000/4500)), 1e-06)
  expect_lt(abs(observed_loss_133_112_199 - (1-1200/2650)), 1e-06)
})

test_that("ordinary sequences are computed correctly", {
  expected_tibble <- tribble(
    ~s,
    "112,199",
    "77,209",
    "209,54",
    "133,112",
    "77,209,54",
  ) %>%
    arrange(s)

  expect_equal(mock_ordinary_sequences %>% select(s), expected_tibble)
})
