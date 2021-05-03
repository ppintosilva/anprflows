quadtree <- function(l = 1000) {
  tibble(
    parent = "0",
    id = "0",
    xmin = 0,
    xmax = l,
    ymin = 0,
    ymax = l
  )
}

filter_leaves <- function(tree, .mode = "in") {
  leaves <- setdiff(tree$id, tree$parent)

  # there is only root node
  if(length(leaves) == 0) {
    leaves = 0
  }

  if(.mode == "out")
    tree %>% dplyr::filter(!(id %in% leaves))
  else
    tree %>% dplyr::filter(id %in% leaves)
}

add_children <- function(tree, nodes) {
  leaves <- tree %>% filter_leaves()

  in_common <- intersect(leaves$id, nodes)

  # if nodes is not a subset of leave nodes then fail
  if(!all.equal(in_common, nodes)) {
    stop("nodes are either/both not in the tree and not leaf nodes")
  }

  node_boxes <- leaves %>% dplyr::filter(id %in% nodes)

  xmin <- node_boxes %>% dplyr::pull(xmin)
  xmax <- node_boxes %>% dplyr::pull(xmax)
  ymin <- node_boxes %>% dplyr::pull(ymin)
  ymax <- node_boxes %>% dplyr::pull(ymax)

  new_nodes <-
    purrr::pmap(
      list(nodes, xmin, xmax, ymin, ymax),
      function(parent, xmin, xmax, ymin, ymax)
        tibble::tribble(
          ~parent, ~id, ~xmin, ~xmax, ~ymin, ~ymax,
          parent, glue::glue("{parent},1"), xmin, xmin+(xmax-xmin)/2, ymin+(ymax-ymin)/2, ymax,
          parent, glue::glue("{parent},2"), xmin, xmin+(xmax-xmin)/2, ymin, ymin+(ymax-ymin)/2,
          parent, glue::glue("{parent},3"), xmin+(xmax-xmin)/2, xmax, ymin, ymin+(ymax-ymin)/2,
          parent, glue::glue("{parent},4"), xmin+(xmax-xmin)/2, xmax, ymin+(ymax-ymin)/2, ymax
        )
    ) %>%
    dplyr::bind_rows()

  dplyr::bind_rows(tree,new_nodes)
}

calc_distance <- function(x1,x2,y1,y2) {
  abs(y2 - y1) + abs(x2-x1)
}

calc_travel_time <- function(d,speed) {
  # m->km / (mp/h->km/h)->km/s
  (d/1000)/(speed*1.60934)*3600
}

roadnet <- function(tree,
                    road_speed = c(70,60,40,30),
                    road_categories = factor(
                      c(4,3,2,1),
                      levels = 1:4,
                      labels = c("Local", "Collector", "Arterial", "Highway")
                    )
) {

  nlevels <- length(road_speed)

  if(nlevels != length(road_categories)) {
    stop("Road speed and categories must have the same length.")
  }

  roads <- tibble(
    level = 1:nlevels,
    speed = road_speed,
    category = road_categories
  )

  raw_nodes <-
    tree %>%
    pivot_longer(
      cols = c(xmin, xmax),
      values_to = "x",
      names_to = "xn"
    ) %>%
    pivot_longer(
      cols = c(ymin, ymax),
      values_to = "y",
      names_to = "yn"
    ) %>%
    select(-c(xn,yn)) %>%
    group_by(id) %>%
    arrange(id,x,y) %>%
    mutate(vertex = row_number()) %>%
    mutate(vertex = dplyr::case_when(vertex == 3 ~ 4L, vertex == 4 ~ 3L, TRUE ~ vertex)) %>%
    bind_rows(filter(., vertex == 1) %>% mutate(vertex = 5)) %>%
    arrange(id, vertex) %>%
    mutate(level = as.integer(stringr::str_count(id, ","))) %>%
    mutate(level = dplyr::case_when(level > nlevels ~ nlevels,
                                    level == 0L ~ 1L,
                                    TRUE ~level)
    ) %>%
    ungroup()

  min_xlevel <-
    raw_nodes %>%
    group_by(x) %>%
    summarise(min_xlevel = min(level), .groups  = "drop")

  min_ylevel <-
    raw_nodes %>%
    group_by(y) %>%
    summarise(min_ylevel = min(level), .groups  = "drop")

  nodes <-
    raw_nodes %>%
    filter_leaves() %>%
    group_by(x,y) %>%
    summarise(
      name = as.character(dplyr::cur_group_id()),
      .groups = "drop"
    )  %>%
    inner_join(min_xlevel, by = "x") %>%
    inner_join(min_ylevel, by = "y") %>%
    rowwise() %>%
    mutate(level = min(min_xlevel, min_ylevel))  %>%
    select(-c(min_xlevel, min_ylevel))

  pre_edges <-
    raw_nodes %>%
    # filter_leaves() %>%
    group_by(x,y) %>%
    mutate(name = as.character(dplyr::cur_group_id())) %>%
    mutate(parent_len = stringr::str_count(parent, ",") + 1) %>%
    dplyr::slice_min(n=1, order_by = parent_len) %>%
    group_by(x,y,parent) %>%
    dplyr::slice(1) %>%
    select(-c(id,vertex))

  edges_x <-
    pre_edges %>%
    group_by(x) %>%
    mutate(min_parent_len = min(parent_len)) %>%
    rowwise() %>%
    mutate(grandparent = str_flatten(str_split(parent, ",")[[1]][1:min_parent_len], ",")) %>%
    group_by(x, grandparent) %>%
    mutate(
      x2 = lead(x),
      y2 = lead(y),
      name2 = lead(name),
    ) %>%
    ungroup() %>%
    filter(!is.na(x2)) %>%
    select(x,y,name,x2,y2,name2)

  edges_y <-
    pre_edges %>%
    group_by(y) %>%
    mutate(min_parent_len = min(parent_len)) %>%
    rowwise() %>%
    mutate(grandparent = str_flatten(str_split(parent, ",")[[1]][1:min_parent_len], ",")) %>%
    group_by(y, grandparent) %>%
    mutate(
      x2 = lead(x),
      y2 = lead(y),
      name2 = lead(name),
    ) %>%
    ungroup() %>%
    filter(!is.na(y2)) %>%
    select(x,y,name,x2,y2,name2)

  edges_uv <-
    bind_rows(edges_x, edges_y) %>%
    ungroup() %>%
    rename(x1 = x, y1 = y, from = name, to = name2) %>%
    mutate(from_name = from, to_name = to) %>%
    inner_join(
      nodes %>% select(-name), by = c("x1" = "x", "y1" = "y")
    ) %>%
    inner_join(
      nodes %>% select(-name), by = c("x2" = "x", "y2" = "y"),
      suffix = c("1", "2")
    ) %>%
    rowwise() %>%
    mutate(level = max(level1, level2)) %>%
    inner_join(roads, by = "level") %>%
    select(from, to, from_name, to_name,
           x1, y1, x2, y2, level, speed, category) %>%
    mutate(d = calc_distance(x1,x2,y1,y2)) %>%
    mutate(dt = calc_travel_time(d,speed)) %>%
    filter(from != to)

  edges_vu <-
    edges_uv %>%
    rename(
      to = from,
      to_name = from_name,
      from = to,
      from_name = to_name,
      x1 = x2,
      x2 = x1,
      y1 = y2,
      y2 = y1
    )

  edges <- bind_rows(edges_uv, edges_vu)

  tidygraph::tbl_graph(
    nodes = nodes,
    edges = edges
  )
}

add_node <- function(G, name, xval, yval, anchor = "") {
  # fail if x or y do not overlap with graph
  # find all nodes which match either the value x or y of the new node

  nodes <- G %>% activate(nodes) %>% as_tibble() %>% mutate(id = row_number())
  edges <- G %>% activate(edges) %>% as_tibble()

  # fail if x or y are outside boundaries
  xmax = max(nodes$x)
  xmin = min(nodes$x)
  ymax = max(nodes$y)
  ymin = min(nodes$y)

  if(xval <= xmin | xval >= xmax | yval >= ymax | yval <= ymin) {
    stop("Node is out of boundaries.")
  }

  match_both <- nodes %>% filter(abs(x - xval) < .0001 & abs(y - yval) < .0001)
  matches_both <- !(nrow(match_both) == 0)
  if(matches_both) {
    stop("Node is placed on top of existing intersection.")
  }

  match_x <- nodes %>% filter(abs(x - xval) < .0001)
  match_y <- nodes %>% filter(abs(y - yval) < .0001)

  matches_x <- !(nrow(match_x) == 0)
  matches_y <- !(nrow(match_y) == 0)

  # if both match_x and match_y are valid
  # then use anchor to decide which one to pick

  new_id <- as.integer(nrow(nodes) + 1)
  new_node <- tibble(id = new_id, name = name, x = xval, y = yval, level = NA)

  candidates_x <-
    match_x %>%
    bind_rows(new_node) %>%
    arrange(y)

  candidates_y <-
    match_y %>%
    bind_rows(new_node) %>%
    arrange(x)

  if(!matches_x & !matches_y) {
    stop("Node is not placed on top of an existing road.")
  } else if(matches_x & matches_y) {
    if(anchor == "x") {
      candidates <- candidates_x
    } else if(anchor == "y") {
      candidates <- candidates_y
    } else {
      stop("Did you forgot to set the anchor?")
    }
  } else if(matches_x) {
    candidates <- candidates_x
  } else if(matches_y) {
    candidates <- candidates_y
  }

  new_edges_uv <-
    candidates %>%
    rename(from = id, from_name = name, x1 = x, y1 = y) %>%
    mutate(
      to = lead(from),
      to_name = lead(from_name),
      x2 = lead(x1),
      y2 = lead(y1)
    ) %>%
    filter(to_name == name | from_name == name)

  new_edges_vu <-
    new_edges_uv %>%
    rename(
      to = from, to_name = from_name,
      from = to, from_name = to_name,
      x1 = x2, x2 = x1,
      y1 = y2, y2 = y1
    )

  new_edges <- bind_rows(new_edges_uv, new_edges_vu)

  old_edge <- edges %>%
    filter(from == new_edges$from[1] & to_name == new_edges$to[2])

  new_edges_road <- new_edges %>%
    select(-level) %>%
    dplyr::bind_cols(
      old_edge %>% select(level,speed,category)
    ) %>%
    mutate(d = calc_distance(x1,x2,y1,y2)) %>%
    mutate(dt = calc_travel_time(d,speed))

  updated_edges <- edges %>%
    filter(!(from == new_edges$from[1] & to_name == new_edges$to[2])) %>%
    filter(!(from == new_edges$to[2] & to_name == new_edges$from[1])) %>%
    bind_rows(new_edges_road)

  updated_nodes <- nodes %>% bind_rows(new_node)

  tidygraph::tbl_graph(
    nodes = updated_nodes,
    edges = updated_edges
  )
}

shortest_path <- function(G, s, t) {

  vs <-
    G %>%
    tidygraph::as.igraph() %>%
    igraph::shortest_paths(from = s, to = t, weights = igraph::edge_attr(., "dt")) %>%
    purrr::pluck("vpath") %>%
    purrr::pluck(1) %>%
    as.integer()

  es <- tibble(
    from = vs,
    to = lead(vs)
  ) %>%
    filter(!is.na(to)) %>%
    mutate(step = row_number())

  G %>%
    activate(edges) %>%
    inner_join(es, by = c("from", "to")) %>%
    arrange(step) %>%
    as_tibble()
}

example_quadtree <- function(l = 10000) {
  quadtree(l) %>%
    # level 1
    add_children("0") %>%
    # level 2
    add_children(c("0,3","0,4")) %>%
    # level 3
    add_children(c("0,3,1","0,3,3","0,3,4","0,4,2","0,4,4")) %>%
    # level 4
    add_children(c("0,3,1,3","0,3,3,3","0,4,2,2","0,4,4,1")) %>%
    # level 5
    add_children(c("0,3,1,3,1","0,3,1,3,4",
                   "0,3,3,3,2","0,3,3,3,3",
                   "0,4,2,2,1","0,4,2,2,3","0,4,2,2,4",
                   "0,4,4,1,4")) %>%
    # level 6
    add_children(c("0,3,1,3,1,2","0,3,1,3,1,4",
                   "0,3,3,3,2,1","0,3,3,3,2,2","0,3,3,3,2,3","0,3,3,3,2,4",
                   "0,3,3,3,3,1","0,3,3,3,3,2","0,3,3,3,3,3","0,3,3,3,3,4",
                   "0,4,2,2,3,1","0,4,2,2,3,2",
                   "0,4,4,1,4,3"))
}

example_roadnet <- function(l = 10000) {
  example_quadtree(l) %>%
    roadnet() %>%
    add_node(name = "A", xval = l/10,     yval = l/2)                 %>%
    add_node(name = "B", xval = 5.5*l/8,  yval = l/2, anchor = "y")   %>%
    add_node(name = "C", xval = 6*l/8,    yval = 6.5*l/8)             %>%
    add_node(name = "D", xval = 6.5*l/8,  yval = 3*l/8, anchor = "y") %>%
    add_node(name = "E", xval = 7.75*l/8, yval = l/2, anchor = "y")   %>%
    add_node(name = "F", xval = 7*l/8,    yval = 2.30*l/8)            %>%
    add_node(name = "G", xval = 7*l/8,    yval = 0.75*l/8, anchor = "x")
}
