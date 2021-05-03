# ----

trip_sequences <- tibble::tribble(
  ~s, ~n,
  "77,209", 400L,
  "209,54", 400L,
  "77,209,54", 200L,
  "133,112", 500L,
  "112,199", 500L
)

distances <- tibble::tribble(
  ~o, ~d, ~distance,
  "77", "209", 3000,
  "209", "54", 1500,
  "77", "54", 4000,
  "133", "112", 950,
  "112", "199", 1700,
  "133", "199", 1200
)

joined_sequences <-
  join_sequences(G_asympt, trip_sequences, method = "e") %>%
  arrange(s)

observed_days <- 2

pre_ordinary_sequences <-
  joined_sequences %>%
  route_utility(distances) %>%
  mutate(rate = n/observed_days, .keep = "unused") %>%
  select(s, l, rate, utility)

result_ordinary_sequences <-
  pre_ordinary_sequences %>%
  ordinary_sequences(min_rate = 30.0, min_utility = .75)

# ----

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

  expect_equal(joined_sequences, expected_tibble)
})

test_that("utility loss works correctly", {
  observed_l2 <-
    pre_ordinary_sequences %>%
    filter(l == 2) %>%
    pull(utility)

  observed_77_209_54 <-
    pre_ordinary_sequences %>%
    filter(s == "77,209,54") %>%
    pull(utility)

  observed_133_112_199 <-
    pre_ordinary_sequences %>%
    filter(s == "133,112,199") %>%
    pull(utility)

  purrr::walk(observed_l2, ~ expect_lt(.x-1, 1e-06))
  expect_lt(abs(observed_77_209_54 - 4000/4500), 1e-06)
  expect_lt(abs(observed_133_112_199 - 1200/2650), 1e-06)
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

  expect_equal(result_ordinary_sequences %>% select(s), expected_tibble)
})
