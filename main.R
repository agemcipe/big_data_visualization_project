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

full_network_df <- read_csv("intermediate/preprocessed_data.csv")
full_author_df <- read_csv("intermediate/authors.csv")
author_df$idx <- seq.int(nrow(author_df)) - 1

# sampled_network_df <- network_df[sample(nrow(network_df), 2000), ]

# select some observations
selected_network_df <- full_network_df[full_network_df$author_id_x < 250, ]

selected_author_df <- author_df[
    author_df$author_id %in% (
        c(
            selected_network_df$author_id_x,
            selected_network_df$author_id_y
        ) %>% unique()),
]
selected_author_df$idx <- seq.int(nrow(selected_author_df)) - 1

joined <- selected_network_df %>%
    inner_join(selected_author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    inner_join(selected_author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))


# simple_network <- simpleNetwork(
#     network_to_display,
#     Source = 1, Target = 2, zoom = TRUE
# )

# simple_network

force_network <- forceNetwork(
    Links = joined,
    Nodes = selected_author_df,
    Source = "idx.x",
    Target = "idx.y",
    Value = "cnt_publications",
    NodeID = "author_name",
    Group = "categories_first",
    zoom = TRUE,
    opacity = 0.8
)

force_network

file_name <- get_variable_name(force_network)
save_widget(force_network, file_name)


#################### TEST ##################
data(MisLinks)
data(MisNodes)

forceNetwork(
    Links = MisLinks, Nodes = MisNodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name",
    Group = "group", opacity = 0.8, zoom = TRUE
)