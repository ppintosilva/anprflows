#' Plot a corridor's attribute as tiles over time. On the y axis,
#' a tile is generated for each segment of the corridor, for each time step,
#' where the bottom and top of the y axis represent the start and end of the
#' corridor, respectively. Segment levels are calculated from location levels.
#' Location levels should be adjusted before the function call.
#'
#' @param corridor_flows corridor od flows [tibble][tibble::tibble-package]
#' @param fill_var var to fill geom_tiles with
#' @param date_breaks string specifying the distance between breaks used by
#' scale_x_datetime
#' @param date_labels string specifying the datetime format of labels used by
#' scale_x_datetime
#' @param location_levels override location levels, otherwise use levels of
#' existing 'o' column of input flows
#'
#' @return ggplot
#'
#' @export
#'
plot_corridor_time <- function(
  corridor_flows,
  fill_var,
  date_breaks = "4 hours",
  date_labels = "%Hh",
  location_levels = NULL
) {
  fill_var = enquo(fill_var)

  if(is.null(location_levels)) {
    location_levels <- levels(corridor_flows$o)
  }

  # create a continuous y axis rather than discrete so that we can use
  # locations as breaks instead of ods: give each od an integer and
  # shift them to minor breaks, so that major breaks are given to locations
  corridor_flows %>%
    mutate(id = match(.data$o, location_levels) + 0.5) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(x = .data$t, y = .data$id, fill = !! fill_var)
    ) +
    ggplot2::scale_x_datetime(
      name = "Time",
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    ggplot2::scale_y_continuous(
      name   = "Location",
      breaks = 1:length(location_levels),
      labels = location_levels
    ) +
    ggplot2::theme_bw()
}

#' Plot a corridor's speed as tiles over time. On the y axis,
#' a tile is generated for each segment of the corridor, for each time step,
#' where the bottom and top of the y axis represent the start and end of the
#' corridor, respectively. Segment levels are calculated from location levels.
#' Location levels should be adjusted before the function call.
#'
#' @param corridor_flows corridor od flows [tibble][tibble::tibble-package]
#' @param fill_var var to fill geom_tiles with
#' @param date_breaks string specifying the distance between breaks used by
#' scale_x_datetime
#' @param date_labels string specifying the datetime format of labels used by
#' scale_x_datetime
#' @param location_levels override location levels, otherwise use levels of
#' existing 'o' column of input flows
#' @param scale_fill_name name of fill scale
#' @param scale_gradientn_colours colours used to generate
#' the speed fill gradient
#' @param scale_limits fill scale limits
#'
#' @return ggplot
#'
#' @export
#'
plot_corridor_time_speed <- function(
  corridor_flows,
  fill_var = .data$mean_speed,
  date_breaks = "4 hours",
  date_labels = "%Hh",
  location_levels = NULL,
  scale_fill_name = "Mean speed",
  scale_gradientn_colours = c("#E46972", "#EFBD7E", "#5B8E54"),
  scale_limits = c(0,80)
) {
  fill_var <- enquo(fill_var)

  corridor_flows %>%
    plot_corridor_time(
      fill_var = !! fill_var,
      date_breaks = date_breaks,
      date_labels = date_labels,
      location_levels = location_levels
    ) +
    ggplot2::scale_fill_gradientn(
      name = scale_fill_name,
      limits = scale_limits,
      colours = scale_gradientn_colours
    )
}


