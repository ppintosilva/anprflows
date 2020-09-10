#' Expand any trip sequence of length 3 or greater, with their subsequences
#' of length 2 or greater.
#'
#' For example, given sequence "A,B,C,D", we generate all of its subsequences
#' of length 2 and 3, resulting in the following sequence set:
#' "A,B", "B,C", "C,D", "A,B,C", "B,C,D", "A,B,C,D"
#'
#' @param seqs a tibble with named column "s": sequence
#' @param separator character separating each element in the sequence
#'
#' @return expanded tibble, with additional column "l" specifying the length of
#' the sequence
#' @export
#'
expand_sequences <- function(seqs, separator = ",") {
  assert_cols(seqs, c("s"))

  seqs <- seqs %>%
    mutate(l = stringr::str_count(.data$s, separator) + 1)

  seqs %>%
    group_by(.data$l >= 3) %>%
    group_map(~{
      if(.y == TRUE) {
        .x %>%
          rowwise() %>%
          # subsequences of length r = 2..l
          mutate(r = list(seq(from = .data$l, to = 2, by = -1))) %>%
          unchop(.data$r) %>%
          rowwise() %>%
          # start and end character indices for each subsequence of length r
          mutate(
            i = list(seq(from = 1, to = .data$l-.data$r+1, by = 1)),
            j = list(seq(from = .data$r, to = .data$l, by = 1))
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
            l = r
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
