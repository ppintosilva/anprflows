# Global variable
NA_LOCATION <- 999999

#' Read one or more flows csv files into R
#'
#' @param filenames Vector of one or more filenames
#' @param datetime_format Format of datetime column.
#'
#' @return A tibble containing "raw" flow data.
#'
#' @export
#'
#' @importFrom magrittr %>% %<>%
read_flows_csv <- function(filenames, datetime_format = '%Y-%m-%d %H:%M:%S')
{
  flows <- tidyr::tibble()

  for (filename in filenames) {
    new_flows <- readr::read_csv(
      file = filename,
      col_names = c(
        "o", "d", "t", "flow", "median_speed", "mean_speed", "sd_speed"
      ),
      col_types = readr::cols(
        o = 'i',
        d = 'i',
        t = readr::col_datetime(format = datetime_format),
        flow = 'i',
        median_speed = 'd',
        mean_speed = 'd',
        sd_speed = 'd'
      )
    )

    flows <- dplyr::bind_rows(flows, new_flows)
  }

  flows %<>%
    dplyr::mutate(
      o = forcats::fct_explicit_na(as.factor(dplyr::na_if(o, NA_LOCATION)),
                                   na_level = "SOURCE"),
      d = forcats::fct_explicit_na(as.factor(dplyr::na_if(d, NA_LOCATION)),
                                   na_level = "SINK")
    )

  # Union of factor levels from origin and destination columns in case
  # they are not the same
  llevels <- union(levels(flows$o), levels(flows$d))

  # Make factor levels the same for location columns
  flows %<>%
    dplyr::mutate(
      o = factor(o, levels = llevels),
      d = factor(d, levels = llevels)
    )

  return(flows)
}
