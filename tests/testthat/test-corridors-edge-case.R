ordinary_sequences <- tibble::tribble(
                               ~s, ~l,
                        "112,199", 2L,
                        "133,112", 2L,
                    "133,112,199", 3L,
                        "135,148", 2L,
                    "135,148,112", 3L,
                "135,148,112,199", 4L,
                    "135,148,133", 3L,
                "135,148,133,112", 4L,
            "135,148,133,112,199", 5L,
                        "148,112", 2L,
                    "148,112,199", 3L,
                        "148,133", 2L,
                    "148,133,112", 3L,
                "148,133,112,199", 4L,
                         "209,54", 2L,
                     "209,54,135", 3L,
                 "209,54,135,148", 4L,
             "209,54,135,148,112", 5L,
             "209,54,135,148,133", 5L,
         "209,54,135,148,133,112", 6L,
     "209,54,135,148,133,112,199", 7L,
                         "54,135", 2L,
                     "54,135,148", 3L,
                 "54,135,148,112", 4L,
                 "54,135,148,133", 4L,
             "54,135,148,133,112", 5L,
         "54,135,148,133,112,199", 6L
     )


ordinary_sequences_2 <- dplyr::add_row(ordinary_sequences, s = "209,54,135,148,112,199", l = 6L)

expected_corridor_2 <- tibble::tribble(
  ~corridor,    ~o,    ~d,
  1L, "209",  "54",
  1L,  "54", "135",
  1L, "135", "148",
  1L, "148", "133",
  1L, "133", "112",
  1L, "148", "112",
  1L, "112", "199"
)

test_that("multiplication works", {

  observed_corridor_1 <- get_corridor_set(ordinary_sequences) %>% arrange(corridor,o,d)
  observed_corridor_2 <- get_corridor_set(ordinary_sequences_2) %>% arrange(corridor,o,d)


  expect_equal(observed_corridor_1 %>% summarise(n = dplyr::n_distinct(corridor)) %>% pull(n), 3)

  expect_true(observed_corridor_1 %>% summarise(n = dplyr::n_distinct(corridor)) %>% pull(n)
              !=
              observed_corridor_2 %>% summarise(n = dplyr::n_distinct(corridor)) %>% pull(n))

  expect_equal(
    observed_corridor_2 %>% arrange(corridor, o, d),
    expected_corridor_2 %>% arrange(corridor, o, d)
  )

})
