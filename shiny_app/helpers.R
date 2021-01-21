#### Define functions
join_links_and_nodes <- function(links_df, author_df) {
  joined <- links_df %>%
    inner_join(
      author_df[, c("author_id", "idx_0", "idx_1")],
      by = c("author_id_x" = "author_id")
    ) %>%
    inner_join(
      author_df[, c("author_id", "idx_0", "idx_1")],
      by = c("author_id_y" = "author_id")
    )
  return(joined)
}

filter_by_author_and_neighbors <- function(author_df, authors, order, full_graph) {
  filtered_author <- author_df %>% filter(
    (author_name %in% authors) | (name %in% authors)
  )
  neighbors_idx_1 <- ego(full_graph, order = order, nodes = filtered_author$idx_1, mindist = 0)
  
  filtered_idx_1 <- as_ids(neighbors_idx_1[[1]])
  for (vertex_list in neighbors_idx_1[-1]) {
    filtered_idx_1 <- c(filtered_idx_1, as_ids(vertex_list))
  }
  
  filtered_author_df <- author_df %>% filter(
    idx_1 %in% filtered_idx_1)
  return(filtered_author_df)
}

prepare_final_links_from_selected_author_df <- function(network_df, author_df) {
  final_network_df <- network_df[
    (network_df$author_id_x %in% author_df$author_id) |
      (network_df$author_id_y %in% author_df$author_id), 
  ]
  
  author_df$idx <- seq.int(nrow(author_df)) - 1
  
  final_network_df <- final_network_df %>%
    inner_join(author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    inner_join(author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))
  
  links_and_nodes <- list("links" = final_network_df, "nodes" = author_df)
  
  return(links_and_nodes)
}

#### Load and prepare full dfs of links and nodes
full_network_df <- read_csv("data/preprocessed_data.csv")
full_author_df <- read_csv("data/authors_joined.csv")
full_author_df$idx_1 <- seq.int(nrow(full_author_df))  # igraph needs 1 index nodes
full_author_df$idx_0 <- full_author_df$idx_1 - 1 # networkD3 needs 0 indexed nodes

full_joined <- join_links_and_nodes(full_network_df, full_author_df)

full_graph <- igraph::graph_from_edgelist(
  as.matrix(full_joined[, c("idx_1.x", "idx_1.y")]),
  directed = FALSE
)
full_comp <- components(full_graph)

full_author_df$membership <- full_comp$membership