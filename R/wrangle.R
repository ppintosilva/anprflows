#' Calculate flows at all unique locations of the road network
#'
#' @param flows A 'raw' flows dataset.
#' @param by_period Whether to group data by time or not.
#'
#' @return flows_l [tibble][tibble::tibble-package]
#'
#' @export
#'
get_flows_l <- function(flows, by_period = TRUE) {
  in_flows <-
    flows %>%
    {
      if(by_period) group_by(., .data$d, .data$t)
      else group_by(., .data$d)
    } %>%
    summarise(`in`= sum(.data$flow)) %>%
    rename(l = .data$d)

  out_flows <-
    flows %>%
    {
      if(by_period) group_by(., .data$o, .data$t)
      else group_by(., .data$o)
    } %>%
    summarise(out = sum(.data$flow)) %>%
    rename(l = .data$o)

  if(by_period)
    join_by <- c("l" = "l", "t" = "t")
  else
    join_by <- c("l" = "l")

  full_join(
      in_flows,
      out_flows,
      by = join_by
    ) %>%
    pivot_longer(
      cols = c("in", "out"),
      names_to = "type",
      values_to = "flow"
    ) %>%
    {
      if(by_period) arrange(., .data$l, .data$t, .data$type)
      else arrange(., .data$l, .data$type)
    } %>%
    mutate(flow = replace_na(.data$flow, 0)) %>%
    ungroup()
}

#' Calculate flow rates for all OD pairs in the road network
#'
#' @param flows A 'raw' flows dataset.
#' @param flows_l Flows at unique locations
#' @param by_period Whether to group data by time or not.
#'
#' @return flows_od [tibble][tibble::tibble-package]
#'
#' @export
#'
get_flows_od <- function(
  flows, flows_l,
  by_period = TRUE
) {
  if(!by_period) {
    stop_if_multiple_time_steps(flows_l)

    join_by_o <- c("o" = "l")
    join_by_d <- c("d" = "l")

    flows <-
      flows %>%
      group_by(.data$o, .data$d) %>%
      #summary_expr Summary expression applied when by_period is FALSE
      summarise(
        flow = sum(.data$flow)
      )
  } else {
    join_by_o <- c("o" = "l", "t" = "t")
    join_by_d <- c("d" = "l", "t" = "t")
  }

  flows %>%
    {
      {nrow(.) -> join_o}
      # total flow coming out of origin
      inner_join(
        .,
        flows_l %>%
          filter(.data$type == "out") %>% rename(flow_o_out = !!sym("flow")),
        by = join_by_o) %>%
        verify(nrow(.) == join_o)
    } %>%
    {
      {nrow(.) -> join_d}
      # total flow entering destination
      inner_join(
        .,
        flows_l %>%
          filter(.data$type == "in") %>% rename(flow_d_in = !!sym("flow")),
        by = join_by_d) %>%
        verify(nrow(.) == join_d)
    } %>%
    mutate(
      rate_o = .data$flow / .data$flow_o_out,
      rate_d = .data$flow / .data$flow_d_in
    ) %>%
    {
      if(by_period) {
        select(., .data$o, .data$d, .data$t, .data$flow_o_out, .data$flow,
               .data$flow_d_in, .data$rate_o, .data$rate_d,
               .data$median_speed, .data$mean_speed, .data$sd_speed)
      }
      else {
        select(., .data$o, .data$d, .data$flow_o_out, .data$flow,
               .data$flow_d_in, .data$rate_o, .data$rate_d)
      }
    } %>%
    ungroup()
}

#' Crop spatial features based on flow data.
#'
#' @param flows_od Trimmed flows$od tibble.
#' @param spatial List of spatial features.
#' @param arterial_highway Only keep arterial edges with this value in the
#' highway attribute.
#' @param bbox_margin Spatial margin added to flows bounding box.
#'
#' @return [list]
#' @export
#'
crop_spatial <- function(
  flows_od,
  spatial,
  arterial_highway = "residential",
  bbox_margin = c(-250,-250,250,250)
) {
  # get unique locations observed in the data
  observed_locations <-
    union(
      flows_od %>% distinct(.data$o) %>% pull(.data$o),
      flows_od %>% distinct(.data$d) %>% pull(.data$d)
    )

  # subset locations by existing flows
  locations <-
    spatial$locations %>%
    filter(.data$id %in% observed_locations)

  # od combinations
  od_combinations <-
    flows_od %>%
    filter(.data$o != "SOURCE", .data$d != "SINK") %>%
    distinct(.data$o, .data$d)

  pairs <-
    suppressWarnings(
      inner_join(
        spatial$pairs,
        od_combinations,
        by = c("o" = "o", "d" = "d")
      )
    )

  # intersection primary and arterial with flows (zoom in)
  bbox <-
    pairs %>%
    pull(.data$geometry) %>%
    sf::st_bbox() +
    bbox_margin

  # looking for arterial network data in
  #   spatial$arterial and spatial$arterial$edges
  if(is.null(spatial$arterial$edges)) {
    arterial <- try_st_crop(spatial$arterial, bbox, "arterial network")
  } else {
    arterial <- try_st_crop(spatial$arterial$edges, bbox, "arterial network")
  }

  arterial <-
    arterial %>%
    {
      if(!is.null(arterial_highway))
        filter(., .data$highway == arterial_highway)
    }

  # looking for primary network data in
  #   spatial$primary and spatial$primary$edges
  if(is.null(spatial$primary$edges)) {
    primary <- try_st_crop(spatial$primary, bbox, "primary network")
  } else {
    primary <- try_st_crop(spatial$primary$edges, bbox, "primary network")
  }

  amenities <- try_st_crop(spatial$amenities, bbox, "amenities")

  return(
    list(
      "locations" = locations,
      "pairs" = pairs,
      "primary" = primary,
      "arterial" = arterial,
      "amenities" = amenities
    )
  )
}


#' Get the total od flow in the network.
#'
#' @param flows_od Flows$od tibble.
#' @param by_period Whether to aggregate total flow by period.
#' @param ignore_sink_source Whether to include ods containing source and sink.
#'
#' @return flows_od [tibble][tibble::tibble-package]
#'
#' @export
#'
get_total_flow <- function(
  flows_od,
  by_period = TRUE,
  ignore_sink_source = TRUE
) {
  flows_od %>%
    {
      if(ignore_sink_source) {
        filter(., .data$o != "SOURCE", .data$d != "SINK")
      } else .
    } %>%
    {
      if(by_period) {
        group_by(., .data$t) %>%
          summarise(total_flow = sum(.data$flow))
      } else {
        sum(.$flow)
      }
    }
}

#' Get the subset of flows whose OD pairs account for the top
#' p proportion of total or period-wise observed traffic flow.
#' Returns all od pairs up to the first od pair that exceeds p.
#'
#' @param flows_od Flows$od tibble.
#' @param p Desired proportion of total traffic.
#' @param by_period Whether to aggregate total flow by period.
#' @param ignore_sink_source Whether to include ods containing source and sink.
#'
#' @return flows_od [tibble][tibble::tibble-package]
#'
#' @export
#'
top_flows <- function(
  flows_od,
  p = .9,
  by_period = TRUE,
  ignore_sink_source = TRUE
){
  epsilon <- 1e-5
  total_flow <- get_total_flow(flows_od, by_period, ignore_sink_source)

  cumflows <-
    flows_od %>%
    {
      if(ignore_sink_source) {
        filter(., .data$o != "SOURCE", .data$d != "SINK")
      } else .
    } %>%
    {
      if(by_period) {
        group_by(., .data$t)
      } else {
        group_by(., .data$o, .data$d) %>%
          summarise(flow = sum(.data$flow)) %>%
          ungroup()
      }
    } %>%
    arrange(desc(.data$flow)) %>%
    {
      if(by_period) {
        {nrow(.) -> expected_rows}
        inner_join(
          ., total_flow,
          by = c("t" = "t")
        ) %>%
        verify(nrow(.) == expected_rows)
      } else .
    } %>%
    mutate(p_cumflow = cumsum(.data$flow)/total_flow)

  # return nearest p_cumflow that is greater than p
  threshold <-
    cumflows %>%
    {
      if(by_period) {
        group_by(., .data$t) %>%
        summarise(pthreshold = first_element_greater(.data$p_cumflow, p))
      }
      else {
        summarise(., elem = first_element_greater(.data$p_cumflow, p)) %>%
          pull(.data$elem)
      }
    }

  cumflows %>%
    {
      if(by_period) {
        {nrow(.) -> expected_rows}
        inner_join(
          ., threshold,
          by = c("t" = "t")
        ) %>%
          verify(nrow(.) == expected_rows)
      } else {
        mutate(., pthreshold = threshold)
      }
    } %>%
    # adding small epsilon to guarantee float comparison returns true for
    # values equal to pthreshold
    filter(.data$p_cumflow <= (.data$pthreshold + epsilon)) %>%
    select(-.data$pthreshold)
}


#' Get the subset of flows whose OD pairs account for the top
#' p proportion of total or period-wise observed traffic flow.
#' Returns all od pairs up to the first od pair that exceeds p.
#'
#' @param flows_od flows at ods [tibble][tibble::tibble-package]
#' @param flows_l flows at locations [tibble][tibble::tibble-package]
#' @param time_resolution temporal resolution of flow tibbles
#' @param locations filter observations occurring at these locations (vector)
#' @param pairs filter observations involving these
#' od pairs [tibble][tibble::tibble-package]
#' @param filter_dayperiod filter observations occurring
#' at this period of the day
#' @param fill_gaps whether to make missing time steps explicit
#' @param filter_weekdays filter observations occurring on these weekdays
#' @param min_t first expected time step, defaults to minimum time in data
#' @param max_t last expected time step, defaults to maximum time in data
#'
#' @return list with updated tibbles of flows_od and flows_l
#'
#' @export
#'
cut_flows <- function(
  flows_od, flows_l,
  time_resolution,
  locations = NULL,
  pairs = NULL,
  filter_dayperiod = "morning",
  fill_gaps = TRUE,
  filter_weekdays = c(1:5),
  min_t = NULL,
  max_t = NULL
) {

  if(is.null(min_t))
    min_t <- min(flows_l$t)

  if(is.null(max_t))
    max_t <- max(flows_l$t)

  all_t <- seq(min_t, max_t, by = time_resolution)

  if(!is.null(locations)) {
    flows_l <- flows_l %>%
      filter(.data$l %in% locations)

    flows_od <- flows_od %>%
      filter(.data$o %in% locations | .data$d %in% locations)
  }

  if(!is.null(pairs)) {
    locs <- union(
      pull(pairs, .data$o),
      pull(pairs, .data$d)
    )

    flows_l <- flows_l %>% filter(.data$l %in% locs)

    flows_levels <- levels(flows_od$o)
    pairs <- pairs %>%
      dplyr::mutate_if(is.character, factor, levels = flows_levels)

    flows_od <- flows_od %>% inner_join(pairs, by = c("o" = "o", "d" = "d"))
  }

  if(!is.null(filter_dayperiod)) {
    flows_l <- flows_l %>% filter(dayperiod(.data$t) == filter_dayperiod)

    flows_od <- flows_od %>% filter(dayperiod(.data$t) == filter_dayperiod)
  }

  if(!is.null(filter_weekdays)) {
    flows_l <- flows_l %>%
      filter(lubridate::wday(.data$t, week_start = 1) %in% filter_weekdays)

    flows_od <- flows_od %>%
      filter(lubridate::wday(.data$t, week_start = 1) %in% filter_weekdays)
  }

  subset_t <- tibble(t = all_t) %>%
    {
      if(!is.null(filter_dayperiod)) {
        filter(., dayperiod(.data$t) == filter_dayperiod)
      }
      else .
    } %>%
    {
      if(!is.null(filter_weekdays)) {
        filter(., lubridate::wday(.data$t, week_start = 1) %in% filter_weekdays)
      }
      else .
    }

  if(fill_gaps) {
    fill_time_gaps(flows_od, flows_l, subset_t)
  } else {
    list("l" = flows_l, "od" = flows_od)
  }
}

#' Make missing time steps explicit (zero vehicle count).
#'
#' @param flows_od flows at ods [tibble][tibble::tibble-package]
#' @param flows_l flows at locations [tibble][tibble::tibble-package]
#' @param expected_time [tibble][tibble::tibble-package] with expected steps
#'
#' @return list with updated tibbles of flows_od and flows_l
#'
#' @export
#'
fill_time_gaps <- function(flows_od, flows_l, expected_time) {

  # pivot wider for filling in missing combinations
  flows_l <- flows_l %>%
    pivot_wider(names_from = "type",
                values_from = "flow",
                names_prefix = "flow_")

  # determine which combinations are missing in the data
  t_dummy <- expected_time %>% mutate(dummy = 0)
  distinct_l <- distinct(flows_l, .data$l) %>% mutate(dummy = 0)
  distinct_od <- distinct(flows_od, .data$o, .data$d) %>% mutate(dummy = 0)

  expected_flows_l  <-
    full_join(t_dummy, distinct_l, by = "dummy") %>%
    select(-.data$dummy)

  expected_flows_od <-
    full_join(t_dummy, distinct_od, by = "dummy") %>%
    select(-.data$dummy)

  # the anti join of expected and actual observations gives us the missing ones
  missing_flows_l <-
    anti_join(expected_flows_l, flows_l,
              by = c("l", "t")) %>%
    mutate(flow_in = 0, flow_out = 0)

  flows_l <- flows_l %>%
    bind_rows(missing_flows_l) %>%
    assertr::verify(
      nrow(expected_flows_l) == nrow(distinct(.))
    ) %>%
    mutate(flow_in = replace_na(.data$flow_in, 0)) %>%
    mutate(flow_out = replace_na(.data$flow_out, 0)) %>%
    arrange(.data$t, .data$l)


  missing_flows_od <-
    anti_join(expected_flows_od, flows_od,
              by = c("o","d", "t")) %>%
    mutate(flow = 0, rate_o = 0, rate_d = 0) %>%
    # fill in missing flow_o_out values
    left_join(flows_l %>%
                select(.data$l, .data$t, .data$flow_out) %>%
                rename(flow_o_out = .data$flow_out),
              by = c("o" = "l", "t" = "t")) %>%
    # fill in missing flow_o_out values
    left_join(flows_l %>%
                select(.data$l, .data$t, .data$flow_in) %>%
                rename(flow_d_in = .data$flow_in),
              by = c("d" = "l", "t" = "t"))

  flows_od <- flows_od %>%
    bind_rows(missing_flows_od) %>%
    arrange(.data$t, .data$o, .data$d)

  # flows_l back to long format
  flows_l <- flows_l %>%
    pivot_longer(cols = starts_with("flow_"),
                 names_to = "type",
                 values_to = "flow") %>%
    mutate(type = if_else(.data$type == "flow_in", "in", "out"))

  list("l" = flows_l, "od" = flows_od)
}
