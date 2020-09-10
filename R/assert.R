#' Assert that cols are present in input tibble
#'
#' @param tib a tibble
#' @param required_cols character vector of columns expected to exist in tib
#'
#' @return logical if assert is true
assert_cols <- function(tib, required_cols) {
  assertthat::assert_that(
    all(required_cols %in% names(tib)),
    msg = glue::glue("Input tibble expects the named columns: {required_cols}"))
}

#' Assert that input is a tibble
#'
#' @param x input
#' @return logical
assert_tibble <- function(x) {
  assertthat::assert_that(tibble::is_tibble(x),
                          msg = "Input must be a tibble.")
}

#' Assert that input is a tidygraph
#'
#' @param x input
#' @return logical
assert_tidygraph <- function(x) {
  assertthat::assert_that(all.equal(class(x), c("tbl_graph", "igraph")),
                          msg = "Input must be a tbl_graph")
}
