context("wrangle")

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

test_that("spatial crop works", {
  # test that obtained spatial bbox is smaller than original bbox
  spatial_raw_flows_1 <-
    suppressWarnings(dplyr::left_join(raw_flows_1, spatial_1$pairs))

  subspatial_1 <-
    spatial_raw_flows_1 %>%
    crop_spatial(spatial_1, bbox_margin = c(-50,-50,50,50))

  # don't compares bboxes of locations and pairs because these are the
  # same in this test case
  old_bboxes <- sapply(spatial_1, sf::st_bbox)[,3:5]
  new_bboxes <- sapply(subspatial_1, sf::st_bbox)[,3:5]

  # xmin and ymin of new bounding boxes should be GREATER (smaller bbox)
  # than xmin and ymin of older (larger) bounding boxes
  expect_true(
    sapply(1:3, function(i) # all cols of new spatial
      sapply(1:2, function(j) # get xmin and ymin
        new_bboxes[j,i] > old_bboxes[j,i]
      )
    ) %>% all()
  )

  # xmax and ymax of new bounding boxes should be SMALLER (smaller bbox)
  # than xmax and ymax of older (larger) bounding boxes
  expect_true(
    sapply(1:3, function(i) # all cols of new spatial
      sapply(3:4, function(j) # get xmax and ymax
        new_bboxes[j,i] < old_bboxes[j,i]
      )
    ) %>% all()
  )
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
