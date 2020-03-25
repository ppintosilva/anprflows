#' Plot a corridor's attribute as tiles over time. On the y axis,
#' a tile is generated for each segment of the corridor, for each time step,
#' where the bottom and top of the y axis represent the start and end of the
#' corridor, respectively. Segment levels are calculated from location levels.
#' Location levels should be adjusted before the function call.
#'
#' @param corridor_flows corridor od flows [tibble][tibble::tibble-package]
#' @param fill_var var to fill geom_tiles with
#' @param unite_sep separator used to unite origin and
#' destination into single od character
#'
#' @return ggplot
#'
#' @export
#'
plot_corridor_time <- function(
  corridor_flows,
  fill_var,
  unite_sep = "-"
) {
  fill_var = enquo(fill_var)

  # calculating segment levels
  segment_levels <- levels(corridor_flows$o) %>% as_tuples(unite_sep)

  corridor_flows %>%
    unite("od", .data$o, .data$d, sep = unite_sep) %>%
    mutate(od = factor(.data$od, levels = segment_levels)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(x = .data$t, y = .data$od, fill = !! fill_var)
    ) +
    ggplot2::scale_x_time(
      name = "Time",
      # breaks = hms::hms(c(0,0,0,0),c(0,0,0,0), 6:9),
      labels = scales::label_time(format = "%Hh")
    ) +
    ggplot2::ylab("od") +
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
#' @param unite_sep separator used to unite origin and
#' destination into a single character
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
  unite_sep = "-",
  scale_fill_name = "Mean speed",
  scale_gradientn_colours = c("#E46972", "#EFBD7E", "#5B8E54"),
  scale_limits = c(0,80)
) {
  fill_var <- enquo(fill_var)

  plot_corridor_time(corridor_flows, !! fill_var, unite_sep) +
    ggplot2::scale_fill_gradientn(
      name = scale_fill_name,
      limits = scale_limits,
      colours = scale_gradientn_colours
    )
}


