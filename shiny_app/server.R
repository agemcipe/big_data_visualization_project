library(shiny)
library(networkD3)
library(htmlwidgets)
library(tidyverse)
library(dplyr)

library(ggplot2)

data(MisLinks)
data(MisNodes)


all_files <- list.files(
    "/home/agemcipe/personal_dump/big_data_visualization_project/intermediate/author_terms_count/",
    full.names = TRUE
)

read_author_terms_count <- function(author_name) {
    clean_author_name <- gsub("[^[:alnum:] ]", "_", author_name)
    fake_author <- "Abadi_M_G_"
    read_csv(
        sample(all_files, 1),
    )
}

make_author_terms_count_plot <- function(author_name) {
    if (is.null(author_name)) {
        return(ggplot(mpg, aes(class)) +
            geom_bar() +
            labs(title = author_name))
    }
    print(author_name)
    at_df <- read_author_terms_count(author_name)
    return(ggplot(head(at_df, 10), aes(y = reorder(term, count), x = count)) +
        geom_col() +
        labs(title = author_name))
}


############ custom java script action to be executed on node click
MyClickScript <- 'Shiny.setInputValue("clicked_node", d.name, {priority: "event"});'

function(input, output) {
    output$force <- renderForceNetwork({
        forceNetwork(
            Links = MisLinks, Nodes = MisNodes, Source = "source",
            Target = "target", Value = "value", NodeID = "name",
            Group = "group", opacity = 1,
            clickAction = MyClickScript
        )
    })
    output$clicked_node <- renderText({
        paste("You have selected", input$clicked_node)
    })

    output$clicked_node_chart <- renderPlot(
        ggplot(mpg, aes(class)) +
            geom_bar() +
            labs(title = input$author)
    )

    output$clicked_node_chart2 <- renderPlot(
        make_author_terms_count_plot(input$clicked_node)
    )
}