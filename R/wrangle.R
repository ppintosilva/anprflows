#' Calculate flows at all unique locations of the road network
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise rename full_join mutate arrange
#' @importFrom dplyr select ungroup
#' @param flows A 'raw' flows dataset.
#' @export
get_flows_l <- function(flows) {
  in_flows <-
    flows %>%
    group_by(d,t) %>%
    summarise(`in`= sum(flow)) %>%
    rename(l = d)

  out_flows <-
    flows %>%
    group_by(o,t) %>%
    summarise(out = sum(flow)) %>%
    rename(l = o)

  full_join(
      in_flows,
      out_flows,
      by = c("l" = "l", "t" = "t")
    ) %>%
    pivot_longer(
      cols = c("in", "out"),
      names_to = "type",
      values_to = "flow"
    ) %>%
    arrange(l,t,type) %>%
    mutate(flow = replace_na(flow, 0)) %>%
    ungroup()
}

#' Calculate flow rates for all OD pairs in the road network
#'
#' @importFrom magrittr %>%
#' @importFrom assertr verify
#' @importFrom dplyr filter rename inner_join mutate select ungroup
#' @param flows A 'raw' flows dataset.
#' @param flows_l Flows at unique locations
#' @export
get_flows_od <- function(flows, flows_l) {
  flows %>%
    {
      {nrow(.) -> join2}
      # total flow coming out of origin
      inner_join(
        .,
        flows_l %>% filter(type == "out") %>% rename(flow_o_out = flow),
        by = c("o" = "l", "t" = "t")) %>%
        verify(nrow(.) == join2)
    } %>%
    {
      {nrow(.) -> join3}
      # total flow entering destination
      inner_join(
        .,
        flows_l %>% filter(type == "in") %>% rename(flow_d_in = flow),
        by = c("d" = "l", "t" = "t")) %>%
        verify(nrow(.) == join3)
    } %>%
    mutate(
      rate_o = flow / flow_o_out,
      rate_d = flow / flow_d_in
    ) %>%
    select(o, d, t, flow_o_out, flow, flow_d_in, rate_o, rate_d,
           median_speed, mean_speed, sd_speed) %>%
    ungroup()
}
