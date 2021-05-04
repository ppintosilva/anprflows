trips <- readr::read_rds(system.file("testdata","trips_sample.rds", package="anprflows"))


test_that("multiplication works", {
  observed_sequences <- count_sequences(trips)

  expected_sequences <- trips %>%
    group_by(k,trip) %>%
    summarise(
      s = stringr::str_c(x1, collapse = ","),
      l = n()
    ) %>%
    group_by(l,s) %>%
    summarise(
      n = n()
    ) %>%
    select(s, l, n) %>%
    filter(l > 1)


  expect_equal(observed_sequences, expected_sequences)
})
