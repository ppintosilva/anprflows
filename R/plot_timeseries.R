#' Plot traffic demand over time, grouped by location.
#'
#' @param flows_l flows_l tibble
#' @param time_breaks breaks for scale_x_datetime
#' @param full_datetime_format datetime format applied to first and last breaks
#' @param partial_datetime_format datetime format applied to elements in between
#' @param point_alpha alpha parameter of point layer
#' @param point_size size parameter of point layer
#' @param line_size size parameter of line layer
#' @param out_flow whether to plot outgoing or incoming flow
#' @param include_source_sink whether to include either source or sink nodes
#' in graph depending on out or incoming flow
#'
#' @export
#'
plot_demand_l <- function(
  flows_l, time_breaks = NULL,
  full_datetime_format = "%y-%m-%d %H:%M",
  partial_datetime_format = "%H:%M",
  point_alpha = .5, point_size = .75,
  line_size = .5, out_flow = TRUE,
  include_source_sink = FALSE
) {
  ylabel <- ifelse(out_flow, "Flow (out)", "Flow (in)")
  aes_y <- ifelse(out_flow, "flow.out", "flow.in")

  flows_l %>%
    {
      if(!include_source_sink)
        filter(., .data$l != "SOURCE", .data$l != "SINK")
      else if(out_flow)
        filter(., .data$l != "SINK")
      else
        filter(., .data$l != "SOURCE")
    } %>%
    pivot_wider(
      names_prefix = "flow.",
      names_from   = "type",
      values_from  = "flow") %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$t,
        y = !!sym(aes_y),
        color = .data$l
      ),
      size = line_size
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$t,
        y = !!sym(aes_y),
        color = .data$l
      ),
      alpha = point_alpha,
      size = point_size
    ) +
    {
      if(!is.null(time_breaks)) {
        ggplot2::scale_x_datetime(
          breaks = time_breaks,
          labels = get_datetime_labels(
            time_breaks, full_datetime_format, partial_datetime_format)
        )
      }
    } +
    ggplot2::xlab("Time") +
    ggplot2::ylab(ylabel) +
    ggplot2::labs(color = "Location") +
    ggplot2::theme_bw()
}

#' Plot traffic demand over time
#'
#' @param flows_od flows_od tibble.
#' @param time_breaks breaks for scale_x_datetime
#' @param full_datetime_format datetime format applied to first and last breaks
#' @param partial_datetime_format datetime format applied to elements in between
#' @param add_points whether to include point layer
#' @param aes_color color aesthetic
#' @param point_alpha alpha parameter of point layer
#' @param point_size size parameter of point layer
#' @param line_size size parameter of line layer
#' @param include_source_sink include or exclude edges from source or to sink
#' nodes
#'
#' @export
#'
plot_demand_od <- function(
  flows_od, time_breaks = NULL,
  full_datetime_format = "%y-%m-%d %H:%M",
  partial_datetime_format = "%H:%M",
  add_points = TRUE,
  aes_color = "od",
  point_alpha = .5, point_size = .75, line_size = .5,
  include_source_sink = FALSE
) {

  flows_od %>%
    {
      if(!include_source_sink)
        filter(., .data$o != "SOURCE", .data$d != "SINK")
      else .
    } %>%
    unite("od", .data$o, .data$d, sep = "->") %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$t,
        y = .data$flow,
        color = !!sym(aes_color)
      ),
      size = line_size
    ) +
    {
      if(add_points) {
        ggplot2::geom_point(
          ggplot2::aes(
            x = .data$t,
            y = .data$flow,
            color = !!sym(aes_color)
          ),
          alpha = point_alpha,
          size = point_size
        )
      }
    } +
    {
      if(!is.null(time_breaks)) {
        ggplot2::scale_x_datetime(
          breaks = time_breaks,
          labels = get_datetime_labels(
            time_breaks, full_datetime_format, partial_datetime_format)
        )
      }
    } +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Flow") +
    {
      if(aes_color != "")
        ggplot2::labs(color = "OD pair")
    } +
    ggplot2::theme_bw()
}

#' Plot traffic speed over time, grouped by od pair.
#'
#' @param flows_od trimmed flows$od tibble
#' @param time_breaks breaks for scale_x_datetime
#' @param full_datetime_format datetime format applied to first and last breaks
#' @param partial_datetime_format datetime format applied to elements in between
#' @param add_points whether to include point layer
#' @param aes_color color aesthetic
#' @param point_alpha alpha parameter of point layer
#' @param point_size size parameter of point layer
#' @param add_ribbon whether to add ribbon representing deviation from the mean
#' @param ribbon_alpha alpha parameter of ribbon layer
#' @param ribbon_fill fill parameter of ribbon layer
#' @param line_size size parameter of line layer
#'
#' @export
#'
plot_speed_od <- function(
  flows_od, time_breaks = NULL,
  full_datetime_format = "%y-%m-%d %H:%M",
  partial_datetime_format = "%H:%M",
  add_points = TRUE,
  aes_color = "od",
  point_alpha = .5, point_size = .75,
  add_ribbon = TRUE, ribbon_alpha = .4, ribbon_fill = "grey90",
  line_size = .5
) {

  flows_od %>%
    # no speed to source and link
    filter(.data$o != "SOURCE", .data$d != "SINK") %>%
    # filter observations with zero flows
    # (there should be a better way to visualise these - missing data)
    filter(!is.na(.data$mean_speed)) %>%
    unite("od", .data$o, .data$d, sep = "->") %>%
    ggplot2::ggplot() +
    {
      if(add_ribbon) {
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = .data$t,
            ymin = .data$mean_speed - .data$sd_speed,
            ymax = .data$mean_speed + .data$sd_speed,
            color = !!sym(aes_color)
          ),
          fill = ribbon_fill,
          linetype = 0,
          alpha = ribbon_alpha
        )}
    } +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$t,
        y = .data$mean_speed,
        color = !!sym(aes_color)
        ),
      size = line_size
    ) +
    {
      if(add_points) {
        ggplot2::geom_point(
          ggplot2::aes(
            x = .data$t,
            y = .data$mean_speed,
            color = !!sym(aes_color)
          ),
          alpha = point_alpha,
          size = point_size
        )
      }
    } +
    {
      if(!is.null(time_breaks)) {
        ggplot2::scale_x_datetime(
          breaks = time_breaks,
          labels = get_datetime_labels(
            time_breaks, full_datetime_format, partial_datetime_format)
        )
      }
    } +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Mean speed") +
    {
      if(aes_color != "")
        ggplot2::labs(color = "OD pair")
    } +
    ggplot2::theme_bw()
}



#' Matrix plot of traffic
#'
#' @param flows_od flows_od tibble
#' @param type type
#' @param add_points whether to include point layer
#' @param aes_color color aesthetic
#' @param include_source_sink whether to include either source or sink nodes
#' in demand plot depending on out or incoming flow
#' @param ... further parameters passed on to plot_demand_od and plot_speed_od
#'
#' @export
#'
plot_ts_matrix <- function(
  flows_od,
  type = "demand",
  add_points = FALSE,
  aes_color = "",
  include_source_sink = TRUE,
  ...
) {

  stopifnot(type %in% c("demand", "speed"))

  nodes <- union(flows_od$o, flows_od$d)

  # (N-1) by (N-1) matrix as sink and source only count as one node
  # sink can not be an origin and source can not be a destination
  nnodes <- length(nodes) - 1

  edges <- flows_od %>%
    distinct(.data$o, .data$d) %>%
    # find i indices
    {
      {nrow(.) -> join_o}
      suppressWarnings(
        inner_join(
          .,
          tibble(l = nodes) %>%
            filter(.data$l != "SINK") %>%
            tibble::rownames_to_column(var = "i") %>%
            mutate(i = as.integer(.data$i)),
          by = c("o" = "l")
        )
      ) %>%
      verify(nrow(.) == join_o)
    } %>%
    # find j indices
    {
      {nrow(.) -> join_d}
      suppressWarnings(
        inner_join(
          .,
          tibble(l = nodes) %>%
            filter(.data$l != "SOURCE") %>%
            tibble::rownames_to_column(var = "j") %>%
            mutate(j = as.integer(.data$j)),
          by = c("d" = "l")
        )
      ) %>%
      verify(nrow(.) == join_d)
    } %>%
    # get 1d index
    mutate(index = (.data$i -1) * nnodes + .data$j)



  make_plot_indices <- edges %>% pull(.data$index)
  empty_plot_indices <- setdiff(1:(nnodes^2), make_plot_indices)

  # initialise list
  plot_list <- vector("list", nnodes^2)

  # ensure that y axes have the same limits
  if(type == "demand") {
    max_y = max(flows_od$flow)
    # round to nearest power of 10
    ymax <- 10*ceiling(max_y/10)
    ymin <- 0
  } else if(type == "speed") {
    max_y <- max(flows_od$mean_speed + flows_od$sd_speed, na.rm = TRUE)
    ymax <- ifelse(max_y < 90, 90, 10*ceiling(max_y/10))
    ymin <- 10
  }

  xmax <- lubridate::ceiling_date(max(flows_od$t), unit = "hour")
  xmin <- lubridate::floor_date(min(flows_od$t), unit = "hour")
  xmid <- xmin + (xmax-xmin)/2
  x1_4th <- xmin + (xmid-xmin)/2
  x3_4th <- xmid + (xmax-xmid)/2

  xbreaks <- c(x1_4th,x3_4th)
  xlabels <- c(format(x1_4th, "%d %b %Hh"), format(x3_4th, "%d %b %Hh"))
  xlims <- c(xmin, xmax)

  # very inefficient but works for now
  for(ind in 1:nnodes^2) {
    if(ind %in% make_plot_indices) {
      edge <- edges %>% filter(.data$index == ind)

      single_flows <- flows_od %>%
        filter(.data$o == edge$o & .data$d == edge$d)

      if(type == "demand") {
        p <- plot_demand_od(single_flows,
                            add_points = add_points,
                            aes_color = aes_color,
                            include_source_sink = include_source_sink,
                            ...)
      }
      else if(type == "speed") {
        if(edge$o == "SOURCE" | edge$d == "SINK") {
          p <- blank_plot()
        } else {
          p <- plot_speed_od(single_flows,
                             add_points = add_points,
                             aes_color = aes_color,
                             ...)
        }
      }
    }
    else {
      p <- blank_plot()
    }

    plot_list[[ind]] <-
      p +
      ggplot2::scale_y_continuous(limits = c(ymin, ymax)) +
      ggplot2::scale_x_datetime(breaks = xbreaks,
                                labels = xlabels,
                                limits = xlims)
  }

  GGally::ggmatrix(
    plot_list,
    nrow = nnodes,
    ncol = nnodes,
    # remove SOURCE from X axis (columns = destination)
    xAxisLabels = setdiff(nodes, c("SOURCE")),
    # remove SINK from X axis (rows = origin)
    yAxisLabels = setdiff(nodes, c("SINK"))
  )
}
