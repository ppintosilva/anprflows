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
    ggplot() +
    geom_line(
      aes(
        x = t,
        y = flow.out,
        color = l
      ),
      size = line_size
    ) +
    geom_point(
      aes(x = t, y = flow.out, color = l),
      alpha = point_alpha,
      size = point_size
    ) +
    {
      if(is.null(time_breaks)) .
      else {
        scale_x_datetime(
          breaks = time_breaks,
          labels = get_time_labels(time_breaks)
        )
      }
    } +
    theme_bw()
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
    unite("od", o, d, sep = "->") %>%
    ggplot() +
    geom_line(
      aes(
        x = t,
        y = flow,
        color = od
      ),
      size = line_size
    ) +
    geom_point(
      aes(x = t, y = flow, color = od),
      alpha = point_alpha,
      size = point_size
    ) +
    {
      if(!is.null(time_breaks)) {
        scale_x_datetime(
          breaks = time_breaks,
          labels = get_time_labels(time_breaks)
        )
      }
    } +
    theme_bw()
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
    unite("od", o, d, sep = "->") %>%
    filter(!is.na(mean_avspeed)) %>%
    ggplot() +
    {
      if(add_ribbon) {
        geom_ribbon(
          aes(
            x = t,
            ymin = mean_avspeed - sd_avspeed,
            ymax = mean_avspeed + sd_avspeed,
            color = od
          ),
          fill = ribbon_fill,
          linetype=0,
          alpha = ribbon_alpha
        )}
    } +
    geom_line(
      aes(
        x = t,
        y = mean_avspeed,
        color = od
        ),
      size = line_size
    ) +
    geom_point(
      aes(
        x = t,
        y = mean_avspeed,
        color = od
      ),
      alpha = point_alpha,
      size = point_size
    ) +
    {
      if(is.null(time_breaks)) .
      else {
        scale_x_datetime(
          breaks = time_breaks,
          labels = get_time_labels(time_breaks)
        )
      }
    } +
    scale_y_continuous("Mean speed") +
    theme_bw()
}
