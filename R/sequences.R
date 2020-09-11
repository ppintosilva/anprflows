#' Expand any trip sequence of length 3 or greater, with their subsequences
#' of length 2 or greater.
#'
#' For example, given sequence "A,B,C,D", we generate all of its subsequences
#' of length 2 and 3, resulting in the following sequence set:
#' "A,B", "B,C", "C,D", "A,B,C", "B,C,D", "A,B,C,D"
#'
#' @param seqs a tibble with named column "s": sequence
#' @param sep character separating each element in the sequence
#'
#' @return expanded tibble, with additional column "l" specifying the length of
#' the sequence
#' @export
#'
expand_sequences <- function(seqs, sep = ",") {
  assert_cols(seqs, c("s"))

  if(!"l" %in% names(seqs)) {
    seqs <- seqs %>%
      mutate(l = stringr::str_count(.data$s, sep) + 1L)
  }

  seqs %>%
    group_by(.data$l >= 3L) %>%
    group_map(~{
      if(.y == TRUE) {
        .x %>%
          rowwise() %>%
          # subsequences of length r = 2..l
          mutate(r = list(seq(from = .data$l, to = 2L, by = -1L))) %>%
          unchop(.data$r) %>%
          rowwise() %>%
          # start and end character indices for each subsequence of length r
          mutate(
            i = list(seq(from = 1L, to = .data$l-.data$r+1, by = 1L)),
            j = list(seq(from = .data$r, to = .data$l, by = 1L))
          ) %>%
          unchop(c(.data$i, .data$j)) %>%
          rowwise() %>%
          # explanation:
          #   str_split - split single string into character vector
          #   [[1]] - output of str_split is a list, of which we have to select
          #           the first element
          #   [c(i:j)] - subset the split string
          #   str_flatten - paste the character vector back into a single string
          mutate(
            s = str_flatten((str_split(.data$s, ",")[[1]][c(i:j)]), ","),
            l = .data$r
          ) %>%
          select(-c(i,j,r))
      }
      else {
        .x
      }
    }) %>%
    bind_rows()
}


#' Sum the number of occurrences across each sequence.
#'
#' @param seqs a tibble of sequences with columns "s","l","n"
#'
#' @return the same tibble with summarised counts
#' @export
#'
reduce_sequences <- function(seqs) {
  assert_cols(seqs, c("s", "l", "n"))

  seqs %>%
  group_by(.data$s) %>%
    summarise(
      n = sum(.data$n),
      l = dplyr::first(.data$l),
      .groups = "drop"
    )
}

#' Obtain the set of observed sequences from a trip dataset, the set of
#' theoretically possible sequences given an ANPR network and compute the
#' set intersection of the two.
#'
#' @param G tidygraph of a ANPR network
#' @param trip_sequences tibble of observed trip sequences
#' @param method method to determine the set of possible trip sequences from
#' a ANPR network. If the network is relatively sparse and of small size then
#' this set can be compute exhaustively (method = "e"). Otherwise if the
#' network is too large and it becomes computationally infeasible to compute the
#' set, we use a heuristic (method = "h") to approximate the resulting subset.
#' @param sep character separating elements of a sequence encoded as a string
#'
#' @return a tibble of sequences and their counts
#' @export
#'
join_sequences <- function(G, trip_sequences, method = "e", sep = ",") {
  assert_tidygraph(G)
  assert_tibble(trip_sequences)

  if(method == "e") {
    # find all paths in G and computer their subsequences
    paths <- all_paths(G) %>%
      filter(.data$node != "SOURCE" & .data$node != "SINK") %>%
      group_by(.data$path) %>%
      summarise(
        s = stringr::str_c(.data$node, collapse = sep),
        l = n(),
        .groups = "drop"
      ) %>%
      filter(.data$l > 1L) %>%
      expand_sequences(sep = sep)

    # compute and reduce subsequences from list of sequences
    seqs <- trip_sequences %>%
      expand_sequences(sep = sep) %>%
      reduce_sequences() %>%
      arrange(.data$s, .data$n) %>%
      select(-.data$l)

    # inner join paths and sequences
    candidates <-
      inner_join(paths, seqs, by = "s") %>%
      distinct(.data$s, .data$l, .data$n)

  }
  else if(method == "h") {
    stop("Not implemented yet.")
  }

  return(candidates)
}
