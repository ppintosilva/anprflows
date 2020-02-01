context("wrangle")

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

test2_filename <-
  system.file("testdata","small_flows_dataset_set2.csv", package="anprflows")

raw_flows_1 <- read_flows_csv(filenames = test1_filename)
raw_flows_2 <- read_flows_csv(filenames = test2_filename)

raw_flows   <-
  read_flows_csv(filenames = c(test1_filename,test2_filename)) %>%
  dplyr::arrange(o,d,t)

test_that("single csv files are read well", {
  expected_names <-
    c("o", "d", "t", "flow", "median_speed", "mean_speed", "sd_speed")

  expect_equal(names(raw_flows_1), expected_names)
  expect_equal(names(raw_flows_2), expected_names)
})

test_that("two csv files are read well", {
  expected_names <-
    c("o", "d", "t", "flow", "median_speed", "mean_speed", "sd_speed")

  # test that reading individual dataframes and binding them
  # is the same as reading both together
  # (except that factors get transformed to characters)
  binded_flows <-
    suppressWarnings(
      dplyr::bind_rows(raw_flows_1, raw_flows_2)
    ) %>%
    dplyr::arrange(o,d,t)

  character_raw_flows <-
    raw_flows %>%
    dplyr::mutate(
      o = as.character(o),
      d = as.character(d)
    )

  expect_equal(binded_flows, character_raw_flows)
})
