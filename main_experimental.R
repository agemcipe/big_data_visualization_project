library(tidyverse)
library(igraph)
library(networkD3)
library(htmlwidgets)
library(here)

output_directory <- here("output")

get_variable_name <- function(var) {
    deparse(substitute(var))
}

save_widget <- function(network, file_name) {
    output_path <- here(
        "output",
        "html_widget",
        paste0(file_name, ".html")
    )
    print(paste0("Saving File to: ", output_path))
    saveWidget(network, file = output_path)
}

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

prepare_final_links <- function(network_df, author_df, for_igraph = FALSE) {
    final_network_df <- network_df[
        (network_df$author_id_x %in% author_df$author_id) |
            (network_df$author_id_y %in% author_df$author_id), 
    ]
    if(for_igraph){
        author_df$idx <- seq.int(nrow(author_df))
    }
    else{
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

full_network_df <- read_csv("intermediate/preprocessed_data.csv")
full_author_df <- read_csv("intermediate/authors_joined.csv")
full_author_df$idx_1 <- seq.int(nrow(full_author_df))  # igraph needs 1 index nodes
full_author_df$idx_0 <- full_author_df$idx_1 - 1 # networkD3 needs 0 indexed nodes

full_joined <- join_links_and_nodes(full_network_df, full_author_df)

full_graph <- igraph::graph_from_edgelist(
    as.matrix(full_joined[, c("idx_1.x", "idx_1.y")]),
    directed = FALSE
)
full_comp <- components(full_graph)

full_author_df$membership <- full_comp$membership

#################### by author name #################
authors <- list("Marwala T.", "Haas Martin") # "David Smith") # "Haas Martin" # "Eisermann M"   # ""
order_of_neighborhood = 5

filtered_author_df2 <- filter_by_author_and_neighbors(full_author_df, authors, order_of_neighborhood, full_graph)

# network_df <- full_network_df
# author_df <- filtered_author_df2
# min_component_size <- 5
# 
# network_df <- prepare_final_links(network_df, author_df, for_igraph = TRUE)[[1]]
# 
# network_df[[1]]
# head(network_df)
# # igraph nodes have to be 1 indexed!
# g <- igraph::graph_from_edgelist(as.matrix(network_df[, c("idx.x", "idx.y")]), directed = FALSE) 
# 
# com <- components(g)
# # hist(component_distribution(g, cumulative = FALSE, mul.size = TRUE)) 
# 
# 
# author_df$membership <- com$membership
# 
# filtered_author_df <- author_df[
#     author_df$membership %in% which(com$csize > min_component_size),
# ]

filtered_author_df <- filter_out_small_connected_components(full_network_df, filtered_author_df2, 5)

links_and_nodes <- prepare_final_links(full_network_df, filtered_author_df)

ColourScale <- 'd3.scaleOrdinal()
            .domain(["cs", "stat"])
           .range(["#FF6900", "#694489"]);'

force_network <- forceNetwork(
    Links = links_and_nodes$links,
    Nodes = links_and_nodes$nodes,
    Source = "idx.x",
    Target = "idx.y",
    Value = "cnt_publications",
    NodeID = "author_name",
    Group = "categories_first",
    # Nodesize = "auth_cnt_publications",
    # radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
    # linkDistance = JS("function(d){return 100/(d.value)}"),
    # linkWidth = JS("function(d) { return Math.pow(d.value, 2); }"),
    zoom = TRUE,
    legend = TRUE,
    # colourScale = JS(ColourScale)
)


file_name <- get_variable_name(force_network)
save_widget(force_network, file_name)


# display force_network in IDE
force_network
