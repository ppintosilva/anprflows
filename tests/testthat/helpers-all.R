# Data ----

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

test2_filename <-
  system.file("testdata","small_flows_dataset_set2.csv", package="anprflows")

spatial1_filename <-
  system.file("testdata","spatial_1.rds", package="anprflows")

raw_flows_1 <- read_flows_csv(filenames = test1_filename)
raw_flows_2 <- read_flows_csv(filenames = test2_filename)

raw_flows   <-
  read_flows_csv(filenames = c(test1_filename,test2_filename)) %>%
  dplyr::arrange(o,d,t)

spatial_1 <- readr::read_rds(spatial1_filename)

flows_l_1 <- get_flows_l(raw_flows_1)
flows_od_1 <- get_flows_od(raw_flows_1, flows_l_1)

flows_l <- get_flows_l(raw_flows)
flows_od <- get_flows_od(raw_flows, flows_l)

# Asymptotic flows

asympt_flows_l_1 <- get_flows_l(raw_flows_1,
                                by_period = FALSE)

asympt_flows_od_1 <- get_flows_od(raw_flows_1, asympt_flows_l_1,
                                  by_period = FALSE)

asympt_flows_l <- get_flows_l(raw_flows,
                              by_period = FALSE)

asympt_flows_od <- get_flows_od(raw_flows, asympt_flows_l,
                                by_period = FALSE)

# Flow networks

G_asympt <- flow_network(
  asympt_flows_od,
  label_subgraphs = TRUE,
  spurious_if_below = c("rate_o" = .10))

G1_asympt <- flow_network(
  asympt_flows_od_1,
  label_subgraphs = TRUE,
  spurious_if_below = c("rate_o" = .10),
  names_as_factors = FALSE
)

G1_asympt_no_spurious <- flow_network(
  asympt_flows_od_1,
  label_subgraphs = TRUE,
  spurious_if_below = NULL,
  names_as_factors = FALSE
)

G1_8am_slice <-
  flows_od_1 %>%
  dplyr::filter(lubridate::hour(t) == 8 & lubridate::minute(t) == 15) %>%
  flow_network(spurious_if_below = NULL)

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
