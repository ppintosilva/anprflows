#' Get and count trip sequences
#'
#' @param df trips dataset with three columns: (k,i,x) i-th trip of vehicle k is sequence of x's
#' @param sep sequence separator character
#'
#' @return tibble with columns (s = sequence, l = length of sequence, n = count)
#' @export
#'
count_sequences <- function(df, sep = ",") {
  col1 <- sym(names(df)[1])
  col2 <- sym(names(df)[2])
  col3 <- sym(names(df)[3])

  df %>%
    group_by({{ col1 }}, {{ col2 }}) %>%
    summarise(
      s = stringr::str_c({{ col3 }}, collapse = sep),
      l = n()
    ) %>%
    group_by(.data$l,.data$s) %>%
    summarise(
      n = n()
    ) %>%
    select(.data$s, .data$l, .data$n) %>%
    filter(.data$l > 1)
}


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
  assert_cols(trip_sequences, c("s", "n"))

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
      # we do not do set intersection here (inner_join) because it may be
      # useful to list explicitly paths that are not observed at all
      left_join(paths, seqs, by = "s") %>%
      mutate(n = replace_na(.data$n, 0L)) %>%
      distinct(.data$s, .data$l, .data$n)

  }
  else if(method == "h") {
    stop("Not implemented yet.")
  }

  return(candidates)
}

#' Calculate the utility loss of each sequence, given the shortest path
#' distance between each pair of locations.
#'
#' @param sequences a tibble of sequences (with columns 's', 'l')
#' @param distances a tibble of distances (with columns 'o', 'd', 'distance')
#' @param .keep_distances keep calculated distances as columns
#'
#' @return tibble with extra column 'utility'
#'
#' @export
route_utility <- function(sequences, distances, .keep_distances = FALSE) {
  assert_tibble(sequences)
  assert_tibble(distances)
  assert_cols(sequences, c("s", "l"))
  assert_cols(distances, c("o", "d", "distance"))

  sequences %>%
    # ensure sequences are unique
    assertr::assert(assertr::is_uniq, .data$s) %>%
    # retrieve first and last elements of each sequence, so that we can
    # compute (1) distance between first and last element and (2) the sum of
    # the distances between each observation pair i,i+1
    group_by(.data$s) %>%
    mutate(camera = .data$s) %>%
    tidyr::separate_rows(.data$camera) %>%
    # get the i,i+1 pairs
    mutate(l1 = .data$camera, l2 = lead(.data$camera)) %>%
    select(-.data$camera) %>%
    # get the 1st and last elements (OD)
    mutate(o = dplyr::first(.data$l1), d = dplyr::last(.data$l1)) %>%
    filter(!is.na(.data$l2)) %>%
    # join with distances (twice)
    left_join(
      distances,
      by = c("o", "d")
    ) %>%
    left_join(
      distances,
      by = c("l1" = "o", "l2" = "d"),
      suffix = c(".od",".el")
    ) %>%
    # crunch back sequences into strings
    summarise(
      d.od = dplyr::first(.data$distance.od),
      d.s = sum(.data$distance.el),
    ) %>%
    mutate(utility = .data$d.od/.data$d.s) %>%
    arrange(.data$s) %>%
    { if(.keep_distances) . else select(., -starts_with("d.")) } %>%
    # recover original columns lost in summarise
    full_join(sequences, by = "s")
}

#' Calculate the subset of trip sequences which are ordinary.
#'
#' @param sequences a tibble of sequences (with columns 's', 'l', 'rate', 'utility')
#' @param G igraph object representing the ANPR flow network
#' @param min_utility minimum accepted route utility
#' @param min_rate minimum accepted daily observation rate: sequences observed
#' at a daily rate below this threshold are rejected.
#' @param .keep_cols whether to keep cols other than 's' and 'l'
#'
#' @return filtered tibble
#'
#' @export

ordinary_sequences <- function(sequences,
                               G,
                               min_rate,
                               min_utility,
                               .keep_cols = FALSE) {
  assert_tibble(sequences)
  assert_cols(sequences, c("s", "l", "rate", "utility"))

  sequences %>%
    mutate(is_path = purrr::map_lgl(.data$s, ~{
      is_simple_path(
        G,
        stringr::str_split(.x, pattern = ",") %>% purrr::pluck(1))

    })) %>%
    filter(.data$is_path & .data$rate > min_rate & .data$utility > min_utility) %>%
    {
      if(.keep_cols) . else select(., .data$s, .data$l)
    }
}


#' Find the subset of ordinary sequences that are not a substring of any other sequence
#'
#' @param ord_sequences tibble of ordinary sequences
#'
#' @return tibble of sequences that are not a substring of any other sequence
#' @export
#'
get_super_sequences <- function(ord_sequences) {

  tibble(
    s1 = ord_sequences$s,
    s2 = ord_sequences$s
  ) %>%
    tidyr::expand(.data$s1,.data$s2) %>%
    filter(.data$s1 != .data$s2) %>%
    mutate(is_subset = stringr::str_detect(.data$s2,.data$s1)) %>%
    group_by(.data$s1) %>%
    summarise(subset_count = sum(.data$is_subset)) %>%
    filter(.data$subset_count == 0) %>%
    pull(.data$s1)
}


#' Compute the cross product of one set of trip sequences and annotate them
#'
#' @param super_sequences tibble of ordinary sequences
#'
#' @return expanded tibble of super_sequences
#' @export
#'
cross_super_sequences <- function(super_sequences) {

  tibble(
    s1 = super_sequences,
    s2 = super_sequences
  ) %>%
    tidyr::expand(.data$s1,.data$s2) %>%
    filter(.data$s1 != .data$s2) %>%
    mutate(
      o1 = purrr::map_chr(.data$s1, ~ seq_first(.x)),
      d1 = purrr::map_chr(.data$s1, ~ seq_last(.x)),
      o2 = purrr::map_chr(.data$s2, ~ seq_first(.x)),
      d2 = purrr::map_chr(.data$s2, ~ seq_last(.x))
    ) %>%
    mutate(
      same_od = (.data$o1 == .data$o2 & .data$d1 == .data$d2),
      s2_has_o1 = purrr::map2_lgl(.x = .data$s2, .y = .data$o1, ~ .y %in% seq_split(.x)),
      s2_has_d1 = purrr::map2_lgl(.x = .data$s2, .y = .data$d1, ~ .y %in% seq_split(.x)),
      has_od = .data$s2_has_o1 & .data$s2_has_d1
    )
}



#' Compute corridor set from ordinary sequences
#'
#' @param ord_sequences tibble of ordinary sequences
#'
#' @return tibble of corridors
#' @export
#'
get_corridor_set <- function(ord_sequences) {

  super_sequences <- get_super_sequences(ord_sequences)
  matrix_super_sequences <- cross_super_sequences(super_sequences)

  groups_super_sequences <-
    matrix_super_sequences %>%
    filter(.data$same_od | .data$has_od) %>%
    group_by(.data$o2,.data$d2) %>%
    mutate(i = dplyr::cur_group_id()) %>%
    group_by(.data$i) %>%
    dplyr::group_modify(~{
      tibble(
        s = union(.x$s1, .x$s2)
      )
    })

  corridors <-
    tibble(
      s = super_sequences
    ) %>%
    anti_join(groups_super_sequences, by = "s") %>%
    mutate(i = 100000L + row_number()) %>%
    bind_rows(groups_super_sequences) %>%
    group_by(.data$i) %>%
    mutate(corridor = dplyr::cur_group_id()) %>%
    ungroup() %>%
    mutate(i = row_number()) %>%
    tidyr::separate_rows(.data$s, sep = ",") %>%
    group_by(.data$i) %>%
    mutate(x2 = lead(.data$s)) %>%
    ungroup() %>%
    filter(!is.na(.data$x2)) %>%
    rename(x1 = .data$s) %>%
    distinct(.data$corridor, .data$x1, .data$x2) %>%
    select(.data$corridor, .data$x1, .data$x2) %>%
    rename(o = .data$x1, d = .data$x2) %>%
    arrange(.data$corridor)

  return(corridors)
}


#' Get corridors from observed sequences (wrapper)
#'
#' @param observed_sequences observed trip sequences as strings
#' @param distances distances tibble
#' @param ndays total number of observed days
#' @param G ANPR flow network igraph
#' @param min_rate minimum observed daily rate
#' @param min_utility minimum route utility
#'
#' @return identified ANPR corridors tibble
#' @export
#'
get_corridors <- function(observed_sequences, distances, ndays, G, min_rate, min_utility) {

  observed_sequences %>%
    expand_sequences() %>%
    reduce_sequences() %>%
    route_utility(distances, .keep_distances = F) %>%
    mutate(rate = .data$n/ndays) %>%
    ordinary_sequences(
      G = G,
      min_rate = min_rate,
      min_utility = min_utility
    ) %>%
    get_corridor_set()
}


#' Get first element of a string sequence
#'
#' @param s string sequence
#' @param ... arguments passed to seq_split
#'
#' @return character
#' @export
seq_first <- function(s, ...) {
  seq_split(s, ...) %>% dplyr::first()
}

#' Get last element of a string sequence
#'
#' @param s string sequence
#' @param ... arguments passed to seq_split
#'
#' @return character
#' @export
seq_last <- function(s, ...) {
  seq_split(s, ...) %>% dplyr::last()
}

#' Split string sequence by sep
#'
#' @param s string sequence
#' @param sep separator character
#'
#' @return character
#' @export
seq_split <- function(s, sep = ",") {
  stringr::str_split(s, sep) %>% purrr::pluck(1)
}



#' Find and return the source node of a graph
#'
#' @param g igraph
#'
#' @return source node, if exists, null otherwise
#' @export
get_source <- function(g) {
  indeg <- igraph::degree(g, mode = "in")
  s <- indeg[which(indeg == 0)]

  if(length(s) == 0)
    return(NA)
  else
    names(s)
}

#' Find and return the sink node of a graph
#'
#' @param g igraph
#'
#' @return sink node, if exists, null otherwise
#' @export
get_sink <- function(g) {
  outdeg <- igraph::degree(g, mode = "out")
  t <- outdeg[which(outdeg == 0)]

  if(length(t) == 0)
    return(NA)
  else
    names(t)
}


#' Determine if the input corridor is valid
#'
#' @param corridor tibble
#'
#' @return logical
#' @export
is_valid_corridor <- function(corridor) {
  # tests: is_dag, one source, one sink, no isolated nodes
  #
  g <- igraph::graph_from_data_frame(corridor)
  deg <- igraph::degree(g, mode = "all")
  s <- get_source(g)
  t <- get_sink(g)

  igraph::is_dag(g) &
    !is.na(s) & length(s) == 1 &
    !is.na(t) & length(t) == 1 &
    sum(deg == 0) == 0
}


is_valid_corridor_set <- function(corridor_set) {
  # tests:
  #  - every corridor is a valid corridor
  #  - no two corridor with same source and sink
  #  -
  #
}
