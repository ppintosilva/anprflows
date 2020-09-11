raw_flows_1 <- read_flows_csv(filenames = test1_filename)
raw_flows_2 <- read_flows_csv(filenames = test2_filename)

flows_l_1 <- get_flows_l(raw_flows_1)
flows_od_1 <- get_flows_od(raw_flows_1, flows_l_1)

asympt_flows_l_1 <- get_flows_l(raw_flows_1,
                                by_period = FALSE)

asympt_flows_od_1 <- get_flows_od(raw_flows_1, asympt_flows_l_1,
                                  by_period = FALSE)


# cut flows
segment_77_209_gaps <- cut_flows(
  flows_od, flows_l,
  time_resolution = "15 min",
  pairs = tibble(o = "77", d = "209"),
  fill_gaps = FALSE)

segment_77_209_extra_row <-
  tibble(l = factor("77", levels(flows_od$o)),
         t = lubridate::ymd_hms("2018-01-02 10:00:00"),
         flow = 23, type = "in")


segment_77_209_nogaps <- cut_flows(
  flows_od,
  suppressWarnings(bind_rows(flows_l, segment_77_209_extra_row)),
  time_resolution = "15 min",
  pairs = tibble(o = "77", d = "209"),
  fill_gaps = TRUE)

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
    mutate(across(c(o,d), as.character)) %>%
    dplyr::arrange(o,d,t)

  character_raw_flows <-
    raw_flows %>%
    dplyr::mutate(
      o = as.character(o),
      d = as.character(d)
    ) %>%
    dplyr::arrange(o,d,t)

  expect_equal(binded_flows, character_raw_flows)
})

test_that("location and od flows are computed well from raw flows", {
  flows_l <-
    get_flows_l(raw_flows)

  expected_names_l <-
    c("l", "t", "type", "flow")

  expect_equal(names(flows_l), expected_names_l)

  flows_od <-
    get_flows_od(raw_flows, flows_l)

  expected_names_od <-
    c("o", "d", "t", "flow_o_out", "flow", "flow_d_in", "rate_o", "rate_d",
      "median_speed", "mean_speed", "sd_speed")

  expect_equal(names(flows_od), expected_names_od)
})

test_that("top flows work", {
  raw_flows_no_source_sink <- raw_flows %>% filter(o != "SOURCE", d != "SINK")

  top100p <- top_flows(raw_flows, p = 1.0, by_period = TRUE)

  expect_true(nrow(top100p) == nrow(raw_flows_no_source_sink))

  top30p <- top_flows(raw_flows, p = .3, by_period = TRUE)

  expect_true(nrow(top30p) < nrow(raw_flows_no_source_sink))

  top100p_total <- top_flows(raw_flows, p = 1.0, by_period = FALSE)

  expect_true(nrow(top100p_total) <= nrow(top100p))

  top30p_total <- top_flows(raw_flows, p = .3, by_period = FALSE)

  expect_true(nrow(top30p_total) <= nrow(top30p))
})

test_that("asymptotic flows_l work", {
  observed_nrows <-
    raw_flows_1 %>%
    get_flows_l(by_period = FALSE) %>%
    nrow()

  expected_nrows <-
    get_flows_l(raw_flows_1) %>%
    distinct(l,type) %>%
    nrow()

  expect_true(expected_nrows == observed_nrows)
})

test_that("asymptotic flows_od work", {
  asympt_flows_l_1 <-
    raw_flows_1 %>%
    get_flows_l(by_period = FALSE)

  # if we couple raw_flows with asympt_flows_l then we should get an error
  expect_error(
    get_flows_od(raw_flows_od_1, asympt_flows_l_1, by_period = TRUE)
  )

  asympt_flows_od_1 <-
    raw_flows_1 %>%
    get_flows_od(asympt_flows_l_1, by_period = FALSE)

  expected_nrows <-
    get_flows_od(raw_flows_1, get_flows_l(raw_flows_1)) %>%
    distinct(o,d) %>%
    nrow()

  observed_nrows <-
    asympt_flows_od_1 %>%
    nrow()

  expect_true(expected_nrows == observed_nrows)
})

test_that("cut flows fill_gaps = FALSE works", {
  # 1 od, morning rush, 15 min resolution
  # 1day, 4 hours * 4 steps per hour
  expected_nrows <- 4*4
  observed_nrows <- segment_77_209_gaps$od %>% nrow()

  expect_true(expected_nrows == observed_nrows)
})

test_that("cut flows fill_gaps = TRUE works", {
  # 1 od, morning rush, 15 min resolution
  # 2 days, 4 hours * 4 steps per hour
  expected_nrows <- 4*4*2
  observed_nrows <- segment_77_209_nogaps$od %>% nrow()

  expect_true(expected_nrows == observed_nrows)

  # flow in second day is zero
  day2_nogaps <-
    segment_77_209_nogaps$od %>% filter(lubridate::day(t) == 2)

  expect_true(day2_nogaps$flow %>% sum() == 0)
})
