#' Plot traffic demand over time, grouped by location.
#'
#' @param flows_l flows_l tibble
#' @param time_breaks breaks for scale_x_datetime
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
          labels = get_time_labels(time_breaks)
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
          labels = get_time_labels(time_breaks)
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
          labels = get_time_labels(time_breaks)
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

