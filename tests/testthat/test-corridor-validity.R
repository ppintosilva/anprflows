valid_corridors <-
  tibble::tribble(
     ~corridor,    ~o,    ~d,
           "1", "100",  "11",
           "1",  "11",  "73",
           "1",  "73",  "61",
           "1", "100",  "69",
           "1",  "69",  "11",
           "2", "101", "100",
           "2", "100",  "69",
           "2",  "69", "110",
           "2", "101", "161",
           "2", "161", "100"
     )


valid_corridors_ord_sequences <-
  tibble::tribble(
                         ~s, ~l,
                   "100,11", 2L,
                "100,11,12", 3L,
                "100,11,73", 3L,
             "100,11,73,61", 4L,
                  "100,143", 2L,
              "100,143,233", 3L,
          "100,143,233,230", 4L,
                   "100,69", 2L,
                "100,69,11", 3L,
             "100,69,11,12", 4L,
         "100,69,11,12,228", 5L,
            "100,69,11,265", 4L,
         "100,69,11,265,83", 5L,
             "100,69,11,73", 4L,
         "100,69,11,73,168", 5L,
          "100,69,11,73,61", 5L,
               "100,69,110", 3L,
            "100,69,110,22", 4L,
        "100,69,110,22,244", 5L,
            "100,69,110,49", 4L,
               "100,69,126", 3L,
           "100,69,126,167", 4L,
               "100,69,235", 3L,
            "100,69,235,58", 4L,
                  "101,100", 2L,
               "101,100,11", 3L,
            "101,100,11,73", 4L,
               "101,100,69", 3L,
            "101,100,69,11", 4L,
         "101,100,69,11,73", 5L,
           "101,100,69,110", 4L,
           "101,100,69,126", 4L,
                  "101,161", 2L,
              "101,161,100", 3L,
           "101,161,100,11", 4L,
        "101,161,100,11,73", 5L,
           "101,161,100,69", 4L,
        "101,161,100,69,11", 5L,
     "101,161,100,69,11,73", 6L,
       "101,161,100,69,110", 5L,
       "101,161,100,69,126", 5L,
               "101,161,64", 3L,
           "101,161,64,146", 4L,
                   "101,64", 2L
     )


invalid_corridors <-
  tibble::tribble(
     ~corridor,    ~o,    ~d,
           "9",  "22", "131",
           "9", "131", "193",
           "9", "114", "193",
           "9", "193",  "22",
          "33",  "22", "131",
          "33", "131", "193",
          "33", "193",  "22",
          "33",  "22", "244",
          "33", "244", "171",
          "51",  "22", "131",
          "51", "131", "193",
          "51", "233", "230",
          "51", "230", "193",
          "51", "193",  "22",
          "51",  "22", "244",
          "68", "160", "146",
          "68", "61", "146",
          "68", "146", "160"
     )


corridors <- bind_rows(
  valid_corridors %>% mutate(expected_valid = T),
  invalid_corridors %>% mutate(expected_valid = F)
) %>%
  group_by(corridor) %>%
  dplyr::group_modify(~{
    tibble(
      is_valid = anprflows::is_valid_corridor(.x %>% select(o,d), ord_seq = valid_corridors_ord_sequences$s),
      g = list(igraph::graph_from_data_frame(.x %>% select(o,d))),
      expected_valid = dplyr::first(.x$expected_valid)
    ) %>%
      mutate(
        s = get_source(g[[1]]),
        t = get_sink(g[[1]])
      )
  }) %>%
  mutate(corridor = as.integer(corridor)) %>%
  arrange(corridor) %>%
  ungroup()

test_that("get source sink works", {

  expected_st <- tibble::tribble(
     ~corridor,    ~s,    ~t,
           "1", "100",  "61",
           "2", "101", "110",
           "9", "114",    NA,
          "33",    NA, "171",
          "51", "233", "244",
          "68",  "61",    NA
     ) %>%
    mutate(corridor = as.integer(corridor)) %>%
    arrange(corridor)

  observed_st <- corridors %>% select(corridor, s, t)


  expect_equal(observed_st, expected_st)

})


test_that("validity is computed correctly", {
  expect_true(all(corridors$is_valid == corridors$expected_valid))
})
