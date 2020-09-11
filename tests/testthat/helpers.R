# Data ----

test1_filename <-
  system.file("testdata","small_flows_dataset_set1.csv", package="anprflows")

test2_filename <-
  system.file("testdata","small_flows_dataset_set2.csv", package="anprflows")

raw_flows   <-
  read_flows_csv(filenames = c(test1_filename,test2_filename)) %>%
  dplyr::arrange(o,d,t)


flows_l <- get_flows_l(raw_flows)
flows_od <- get_flows_od(raw_flows, flows_l)

# Asymptotic flows

asympt_flows_l <- get_flows_l(raw_flows,
                              by_period = FALSE)

asympt_flows_od <- get_flows_od(raw_flows, asympt_flows_l,
                                by_period = FALSE)

# Flow networks
G_asympt <- flow_network(
  asympt_flows_od,
  label_subgraphs = TRUE,
  spurious_if_below = c("rate_o" = .10))
