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
    filter(.data$o != "SOURCE", .data$d != "SINK") %>%
    pull(.data$geometry) %>%
    sf::st_bbox() +
    bbox_margin

  # looking for arterial network data in
  #   spatial$arterial and spatial$arterial$edges
  if(is.null(spatial$arterial$edges)) {
    arterial <- suppressWarnings(sf::st_crop(spatial$arterial, bbox))
  } else {
    arterial <- suppressWarnings(sf::st_crop(spatial$arterial$edges, bbox))
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
    primary <- suppressWarnings(sf::st_crop(spatial$primary, bbox))
  } else {
    primary <- suppressWarnings(sf::st_crop(spatial$primary$edges, bbox))
  }

  amenities <- suppressWarnings(sf::st_crop(spatial$amenities, bbox))

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
