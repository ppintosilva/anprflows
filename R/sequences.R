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
#' @examples
expand_sequences <- function(seqs, separator = ",") {
  required_cols <- c("s")

  assertthat::assert_that(
    all(required_cols %in% names(seqs)),
    msg = glue::glue("Input tibble must have named columns: {required_cols}"))

  seqs <- seqs %>%
    mutate(l = stringr::str_count(s, separator) + 1)

  seqs %>%
    group_by(l >= 3) %>%
    group_map(~{
      if(.y == TRUE) {
        .x %>%
          rowwise() %>%
          # subsequences of length r = 2..l
          mutate(r = list(seq(from = l, to = 2, by = -1))) %>%
          unchop(r) %>%
          rowwise() %>%
          # start and end character indices for each subsequence of length r
          mutate(
            i = list(seq(from = 1, to = l-r+1, by = 1)),
            j = list(seq(from = r, to = l, by = 1))
          ) %>%
          unchop(c(i,j)) %>%
          rowwise() %>%
          # explanation:
          #   str_split - split single string into character vector
          #   [[1]] - output of str_split is a list, of which we have to select
          #           the first element
          #   [c(i:j)] - subset the split string
          #   str_flatten - paste the character vector back into a single string
          mutate(
            s = str_flatten((str_split(s, ",")[[1]][c(i:j)]), ","),
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
