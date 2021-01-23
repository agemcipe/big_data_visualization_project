library(tidyr)

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
    idx_1 %in% filtered_idx_1
  )
  filtered_author_df <- filtered_author_df %>%
    mutate(selected = replace(
      selected,
      (author_name %in% authors) | (name %in% authors),
      "Selected"
    ))
  return(filtered_author_df)
}

group_email_domain_endings <- function(author_df) {
  if (n_distinct(author_df$email_domain_end) <= 10) {
    return(author_df)
  }

  save <- author_df %>%
    group_by(email_domain_end) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    filter(email_domain_end != "\'N.A.\'")


  sparse_endings <- unique(tail(save$email_domain_end, -8))
  new_author_df <- author_df %>%
    mutate(email_domain_end = replace(email_domain_end, email_domain_end %in% sparse_endings, "\'Diverse\'"))

  return(new_author_df)
}

prepare_final_links <- function(network_df, author_df, for_igraph = FALSE) {
  final_network_df <- network_df[
    (network_df$author_id_x %in% author_df$author_id) |
      (network_df$author_id_y %in% author_df$author_id),
  ]
  if (for_igraph) {
    author_df$idx <- seq.int(nrow(author_df))
  }
  else {
    author_df$idx <- seq.int(nrow(author_df)) - 1
  }

  final_network_df <- final_network_df %>%
    inner_join(author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    inner_join(author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))

  links_and_nodes <- list("links" = final_network_df, "nodes" = author_df)

  return(links_and_nodes)
}

filter_out_small_connected_components <- function(network_df, author_df, min_component_size) {
  network_df <- prepare_final_links(network_df, author_df, for_igraph = TRUE)[[1]]

  head(network_df)
  # igraph nodes have to be 1 indexed!
  g <- igraph::graph_from_edgelist(as.matrix(network_df[, c("idx.x", "idx.y")]), directed = FALSE)

  com <- components(g)
  # hist(component_distribution(g, cumulative = FALSE, mul.size = TRUE))


  author_df$membership <- com$membership

  filtered_author_df <- author_df[
    author_df$membership %in% which(com$csize >= min_component_size),
  ]
  return(filtered_author_df)
}

### helpers for terms count

make_author_terms_count_plot <- function(lookup_author_name) {
  if (is.null(lookup_author_name)) {
    return(ggplot() +
      theme_void())
  }
  at_df <- full_author_terms_count_df %>% filter(
    author_name %in% lookup_author_name
  )

  if (nrow(at_df) == 0) {
    return(ggplot() +
      theme_void())
  }

  if (length(lookup_author_name > 1)) {
    at_df <- at_df %>%
      group_by(term) %>%
      summarise(
        count = sum(count)
      )
  }

  at_df <- at_df %>%
    arrange(desc(count)) %>%
    slice_head(n = 20)

  return(
    ggplot(at_df, aes(y = reorder(term, count), x = count)) +
      geom_col() +
      ylab("term") +
      labs(title = paste0("Word Stem Frequency in Papers by ", paste0(lookup_author_name, collapse = ", ")))
  )
}

make_compare_author_chart <- function(selected_authors, clicked_author) {
  authors <- c(selected_authors, clicked_author)
  num_authors <- length(c(selected_authors, clicked_author))

  if (is.null(selected_authors) || (num_authors < 2)) {
    return(ggplot() +
      theme_void())
  }

  at_df <- full_author_terms_count_df %>% filter(
    author_name %in% c(selected_authors, clicked_author)
  )

  # Filter only words that are used by more than half of the authors

  terms_to_display <- at_df %>%
    group_by(term) %>%
    summarise(num_author = n_distinct(author_name)) %>%
    filter(num_author > (num_authors / 2)) %>%
    select(term)

  # at_df <- at_df %>% filter(term %in% terms_to_display$term)

  at_df <- tidyr::crossing(authors, terms_to_display) %>%
    rename(author_name = authors) %>%
    left_join(at_df) %>%
    replace_na(list(count = 0)) %>%
    select("author_name", "term", "count")


  if (nrow(at_df) == 0) {
    return(ggplot() +
      theme_void())
  }

  return(
    ggplot(at_df, aes(
      x = as.factor(author_name),
      y = reorder(term, desc(term)),
      fill = count
    )) +
      geom_tile(colour = "white") +
      scale_fill_gradient(
        high = "#00a2ff", low = "#ffffff", limits = c(0, NA)
      ) +
      ylab("term") +
      xlab("author_name") +
      labs(title = "Overlapping Word Stems") +
      theme(panel.background = element_blank())
  )
}

#### Load and prepare full dfs of links and nodes
full_network_df <- read_csv("data/links.csv")
full_author_df <- read_csv("data/authors.csv")
full_author_terms_count_df <- read_csv("data/author_terms_count.csv")

# igraph needs 1 index nodes
full_author_df$idx_1 <- seq.int(nrow(full_author_df))
# networkD3 needs 0 indexed nodes
full_author_df$idx_0 <- full_author_df$idx_1 - 1

full_author_df$selected <- rep("Unselected", nrow(full_author_df))
full_author_df <- full_author_df %>%
  mutate(email_domain_end = replace_na(email_domain_end, "\'N.A.\'"))

full_joined <- join_links_and_nodes(full_network_df, full_author_df)

full_graph <- igraph::graph_from_edgelist(
  as.matrix(full_joined[, c("idx_1.x", "idx_1.y")]),
  directed = FALSE
)
full_comp <- components(full_graph)

full_author_df$membership <- full_comp$membership