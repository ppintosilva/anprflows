#----

raw_flows_1 <- read_flows_csv(filenames = test1_filename)

spatial1_filename <-
  system.file("testdata","spatial_1.rds", package="anprflows")

spatial_1 <- readr::read_rds(spatial1_filename)

# ----

test_that("spatial crop works", {
  # test that obtained spatial bbox is smaller than original bbox
  # in order for this statement to work, we have to first load the sf library
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
