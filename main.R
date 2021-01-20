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
# sampled_network_df <- network_df[sample(nrow(network_df), 2000), ]


# select some observations
#################### by author name #################
search_name <- "David Smith" # "Haas Martin" # "Eisermann M"   # "Marwala T."
preselected_author <- full_author_df %>% filter(
    (author_name == search_name) | (name == search_name)
)

if (nrow(preselected_author) == 0) {
    print("No authors found... trying regex search")
    preselected_author <- full_author_df %>% filter(
        grepl(search_name, author_name) | grepl(search_name, name)
    )
}

if (nrow(preselected_author) > 1) {
    print("Warning! More than one author found")
    preselected_author
}


###################### BY category ########################
# preselected_author <- full_author_df[full_author_df$categories_first == "cs",]


##################### Randomly ########################
# preselected_author <- full_author_df[sample(nrow(full_author_df), 500), ]

#################### Subselect network ####################
selected_network_df <- full_network_df[
    (full_network_df$author_id_x %in% preselected_author$author_id) |
    (full_network_df$author_id_y %in% preselected_author$author_id), 
]

# other way: every node that is in the same component
authors_in_same_comp <- full_author_df[
    full_author_df$membership %in% preselected_author$membership,
]

selected_network_df <- full_network_df[
    (full_network_df$author_id_x %in% authors_in_same_comp$author_id) |
    (full_network_df$author_id_y %in% authors_in_same_comp$author_id), 
]

# other way: filter by neighborhood: ego https://igraph.org/r/doc/ego.html


################### Calculate number of publications... this is wrong.... #########
author_id_x_cnt = selected_network_df %>%
    group_by(author_id_x) %>%
    summarise(auth_cnt_publications = sum(cnt_publications)) %>%
    rename(author_id = author_id_x)
author_id_y_cnt = selected_network_df %>%
    group_by(author_id_y) %>%
    summarise(auth_cnt_publications = sum(cnt_publications)) %>%
    rename(author_id = author_id_y)

author_pub_cnt = author_id_x_cnt %>%
    bind_rows(author_id_y_cnt) %>%
    group_by(author_id) %>%
    summarise(auth_cnt_publications = sum(auth_cnt_publications))

selected_author_df <- full_author_df[
    full_author_df$author_id %in% (
        c(
            selected_network_df$author_id_x,
            selected_network_df$author_id_y
        ) %>% unique()),
]
selected_author_df <- selected_author_df %>%
    inner_join(author_pub_cnt)

selected_author_df$idx <- seq.int(nrow(selected_author_df)) # - 1

joined <- selected_network_df %>%
    inner_join(selected_author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    inner_join(selected_author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))

############# filter by cluster size

# igraph nodes have to be 1 indexed!
g <- igraph::graph_from_edgelist(as.matrix(joined[, c("idx.x", "idx.y")]), directed = FALSE) 

# number of edges
gsize(g)

# number of nodes
length(V(g))

# density = edges / nodes

gsize(g) / length(V(g)) # should be between 3 and 4 ?

com <- components(g)
# hist(component_distribution(g, cumulative = FALSE, mul.size = TRUE)) 


selected_author_df$membership <- com$membership

# FILTERING ONLY CONNECTED COMPONENTS
min_component_size = 1

filtered_selected_author_df <- selected_author_df[selected_author_df$membership %in% which(com$csize > min_component_size),]

filtered_selected_author_df$idx <- seq.int(nrow(filtered_selected_author_df)) - 1

filtered_joined <- selected_network_df %>%
    inner_join(filtered_selected_author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    inner_join(filtered_selected_author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))


ColourScale <- 'd3.scaleOrdinal()
            .domain(["cs", "stat"])
           .range(["#FF6900", "#694489"]);'

force_network <- forceNetwork(
    Links = filtered_joined,
    Nodes = filtered_selected_author_df,
    Source = "idx.x",
    Target = "idx.y",
    Value = "cnt_publications",
    NodeID = "author_name",
    Group = "categories_first",
    Nodesize = "auth_cnt_publications",
    radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
    linkDistance = JS("function(d){return 100/(d.value)}"),
    linkWidth = JS("function(d) { return Math.pow(d.value, 2); }"),
    zoom = TRUE,
    legend = TRUE,
    # colourScale = JS(ColourScale)
)


file_name <- get_variable_name(force_network)
save_widget(force_network, file_name)


force_network
#################### TEST ##################
data(MisLinksagemcipe/data_processes_mlcolonoscopy)
data(MisNodes)

forceNetwork(
    Links = MisLinks, Nodes = MisNodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name",
    Group = "group", zoom = TRUE
)

################## Clusters #################
