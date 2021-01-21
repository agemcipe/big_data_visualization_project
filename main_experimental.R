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

links_from_author_df <- function(author_df) {
    
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
search_name <- "Marwala T." # "David Smith" # "Haas Martin" # "Eisermann M"   # ""
preselected_author <- full_author_df %>% filter(
    (author_name == search_name) | (name == search_name)
)

V(full_graph)
neighbors_idx_1 <- ego(full_graph, order = 3, nodes = preselected_author$idx_1, mindist = 0)
preselected_author_df <- full_author_df %>% filter(
    idx_1 %in% neighbors_idx_1[[1]])

# every author in the preselected_author_df
selected_network_df <- full_network_df[
    (full_network_df$author_id_x %in% preselected_author_df$author_id) |
        (full_network_df$author_id_y %in% preselected_author_df$author_id), 
]

preselected_author_df$idx <- seq.int(nrow(preselected_author_df)) # - 1

joined <- selected_network_df %>%
    inner_join(preselected_author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    inner_join(preselected_author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))


ColourScale <- 'd3.scaleOrdinal()
            .domain(["cs", "stat"])
           .range(["#FF6900", "#694489"]);'

force_network <- forceNetwork(
    Links = joined,
    Nodes = preselected_author_df,
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
