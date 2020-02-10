# Functions ---------------------------

#' Plot traffic demand over time, grouped by location.
#'
#' @param flows_l Trimmed flows$l tibble.
#' @param time_breaks Breaks for scale_x_datetime
#' @param point_alpha Alpha parameter of point layer
#' @param point_size Size parameter of point layer
#' @param line_size Size parameter of line layer
#'
#' @export
#'
#' @importFrom tidyr pivot_wider unite
#'
plot_demand_l <- function(
  flows_l, time_breaks = NULL,
  point_alpha = .5, point_size = .75,
  line_size = .5
) {
  flows_l %>%
    pivot_wider(
      names_prefix = "flow.",
      names_from   = "type",
      values_from  = "flow") %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$t,
        y = .data$flow.out,
        color = .data$l
      ),
      size = line_size
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$t,
        y = .data$flow.out,
        color = .data$l
      ),
      alpha = point_alpha,
      size = point_size
    ) +
    {
      if(is.null(time_breaks)) .
      else {
        ggplot2::scale_x_datetime(
          breaks = time_breaks,
          labels = get_time_labels(time_breaks)
        )
      }
    } +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Flow (out)") +
    ggplot2::labs(color = "Location") +
    ggplot2::theme_bw()
}

#' Plot traffic demand over time, grouped by od pair.
#'
#' @param flows_od Trimmed flows$od tibble.
#' @param time_breaks Breaks for scale_x_datetime
#' @param point_alpha Alpha parameter of point layer
#' @param point_size Size parameter of point layer
#' @param line_size Size parameter of line layer
#' @export
plot_demand_od <- function(
  flows_od, time_breaks = NULL,
  point_alpha = .5, point_size = .75,
  line_size = .5
) {
  # n_distinct_ods <-
  #   flows_od %>% distinct(o,d) %>% summarise(total = n()) %>% pull(total)
  #
  # stopifnot(n_distinct_ods <= 10)

  flows_od %>%
      unite("od", .data$o, .data$d, sep = "->") %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$t,
        y = .data$flow,
        color = .data$od
      ),
      size = line_size
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$t,
        y = .data$flow,
        color = .data$od
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
    ggplot2::ylab("Flow (out)") +
    ggplot2::labs(color = "OD pair") +
    ggplot2::theme_bw()
}

#' Plot traffic speed over time, grouped by od pair.
#'
#' @param flows_od Trimmed flows$od tibble.
#' @param time_breaks Breaks for scale_x_datetime
#' @param point_alpha Alpha parameter of point layer
#' @param point_size Size parameter of point layer
#' @param add_ribbon Whether to add ribbon representing deviation from the mean
#' @param ribbon_alpha Alpha parameter of ribbon layer
#' @param ribbon_fill Fill parameter of ribbon layer
#' @param line_size Size parameter of line layer
#'
#' @export
#'
plot_speed_od <- function(
  flows_od, time_breaks = NULL,
  point_alpha = .5, point_size = .75,
  add_ribbon = TRUE, ribbon_alpha = .4, ribbon_fill = "grey90",
  line_size = .5
) {
  flows_od %>%
    unite("od", .data$o, .data$d, sep = "->") %>%
    filter(!is.na(.data$mean_avspeed)) %>%
    ggplot2::ggplot() +
    {
      if(add_ribbon) {
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = .data$t,
            ymin = .data$mean_avspeed - .data$sd_avspeed,
            ymax = .data$mean_avspeed + .data$sd_avspeed,
            color = .data$od
          ),
          fill = ribbon_fill,
          linetype = 0,
          alpha = ribbon_alpha
        )}
    } +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$t,
        y = .data$mean_avspeed,
        color = .data$od
        ),
      size = line_size
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$t,
        y = .data$mean_avspeed,
        color = .data$od
      ),
      alpha = point_alpha,
      size = point_size
    ) +
    {
      if(is.null(time_breaks)) .
      else {
        ggplot2::scale_x_datetime(
          breaks = time_breaks,
          labels = get_time_labels(time_breaks)
        )
      }
    } +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Mean speed") +
    ggplot2::labs(color = "OD pair") +
    ggplot2::theme_bw()
}
