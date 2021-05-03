#' Does the simple path exist in G
#'
#' @param G igraph
#' @param vs vertex sequence vector
#'
#' @return boolean
#' @export
#'
is_simple_path <- function(G, vs) {
  stopifnot(igraph::is_igraph(G))

  path <- tryCatch(
    igraph::E(G, path = igraph::V(G)[vs]),
    error = function(c) NULL
  )

  !is.null(path)
}
