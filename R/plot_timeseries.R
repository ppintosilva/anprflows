# Functions ---------------------------

#' Plot traffic demand over time, grouped by location.
#'
#' @param flows Trimmed flows tibble.
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_datetime theme_bw
#' ggtitle
#' @param flows_l Trimmed flows tibble.
#' @param point_alpha Alpha parameter of point layer
#' @param point_size Size parameter of point layer
#' @export
plot_demand_l <- function(
  flows_l, time_breaks = NULL, point_alpha = .5, point_size = .75, title = NA)
{
  flows_l %>%
    pivot_wider(
      names_prefix = "flow.",
      names_from   = "type",
      values_from  = "flow") %>%
    ggplot() +
    geom_line(
      aes(x = t, y = flow.out, color = l)
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
    ggtitle(title) +
    theme_bw()
}

#' Plot traffic demand over time, grouped by od pair.
#'
#' @param flows Trimmed flows tibble.
#' @export
plot_demand_od <- function() {
  return(TRUE)
}

#' Plot traffic speed over time, grouped by od pair.
#'
#' @param flows Trimmed flows tibble.
#' @export
plot_speed_od <- function() {
  return(TRUE)
}
