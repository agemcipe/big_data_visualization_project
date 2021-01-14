library(tidyverse)
library(igraph)
library(networkD3)
library(htmlwidgets)
library(here)

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
output_directory <- here("output")

network_df <- read_csv("intermediate/preprocessed_data.csv")
author_df <- read_csv("intermediate/authors.csv")

author_df$group <- 0

sampled_network_df <- network_df[sample(nrow(network_df), 1000), ]

simple_network <- simpleNetwork(sampled_network_df, Source = 1, Target = 2, zoom = TRUE)

force_network <- forceNetwork(
    Links = network_df,
    Nodes = author_df,
    Source = "author_id_x",
    Target = "author_id_y",
    Value = "cnt_publications",
    NodeID = "author_id",
    Group = "group",
    zoom = TRUE
)


file_name <- get_variable_name(simple_network)
save_widget(simple_network, file_name)