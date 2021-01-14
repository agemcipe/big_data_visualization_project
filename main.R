library(tidyverse)
library(igraph)
library(networkD3)
library(htmlwidgets)
library(here)

get_variable_name <- function(var) {
    deparse(substitute(var))
}

save_widget <- function(network) {
    saveWidget(network,
        file = here(
            "output",
            "html_widget",
            paste0(get_variable_name(network), ".html")
        )
    )
}
output_directory <- here("output")

network_df <- read_csv("output/preprocessed_data.csv")
author_df <- read_csv("output/authors.csv")

data <- igraph::graph_from_data_frame(d = network_df[1:100, ], directed = FALSE)

my_data <- as.data.frame(network_df)
simple_network <- simpleNetwork(my_data, Source = 1, Target = 2, zoom = TRUE)

force_network <- forceNetwork(
    Links = my_data,
    Nodes = author_df,
    Source = "author_id_x",
    Target = "author_id_y",
    Value = "cnt_publications",
    NodeID = "author_id",
    Group = "group",
    zoom = TRUE
)


save_widget(force_network)