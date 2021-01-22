library(shiny)
library(tidyverse)
library(igraph)
library(networkD3)
source("helpers.R")

#### For testing
data(MisLinks) 
data(MisNodes)





#### Define server logic required to draw output
server <- function(input, output) {
  
  output$example <- renderForceNetwork(forceNetwork(Links = MisLinks, Nodes = MisNodes,
                                                    Source = "source", Target = "target",
                                                    Value = "value", NodeID = "name",
                                                    Group = "group", opacity = 0.8))
  
  authors <- reactive({as.list(strsplit(input$authors,","))
    
  })
  
  # Filter by author and select neighbors
  filtered_author_df <- reactive({
    # author_df = full_author_df
    # order = input$neighbors
    filter_by_author_and_neighbors(full_author_df, authors()[[1]],input$neighbors, full_graph) # "Marwala T.,Haas Martin"
    # filtered_author <- author_df %>% filter(
    #   (author_name %in% authors) | (name %in% authors)
    # )
    # neighbors_idx_1 <- ego(full_graph, order = order, nodes = filtered_author$idx_1, mindist = 0)
    # 
    # filtered_idx_1 <- as_ids(neighbors_idx_1[[1]])
    # for (vertex_list in neighbors_idx_1[-1]) {
    #   filtered_idx_1 <- c(filtered_idx_1, as_ids(vertex_list))
    # }
    # 
    # filtered_author_df <- author_df %>% filter(
    #   idx_1 %in% filtered_idx_1)
  })
  
  links <- reactive({
    # network_df = full_network_df
    # author_df = filtered_author_df
    links_and_nodes <- prepare_final_links_from_selected_author_df(full_network_df, filtered_author_df())
    links_and_nodes$links
    # final_network_df <- network_df[
    #   (network_df$author_id_x %in% author_df$author_id) |
    #     (network_df$author_id_y %in% author_df$author_id), 
    # ]
    # 
    # author_df$idx <- seq.int(nrow(author_df)) - 1
    # 
    # final_network_df <- final_network_df %>%
    #   inner_join(author_df[, c("author_id", "idx")], by = c("author_id_x" = "author_id")) %>%
    #   inner_join(author_df[, c("author_id", "idx")], by = c("author_id_y" = "author_id"))
    # 
    # links_and_nodes <- list("links" = final_network_df, "nodes" = author_df)
    })
  output$net <- renderForceNetwork(forceNetwork(
    Links = links(),
    Nodes = filtered_author_df(),
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
  )
  
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Author
      textInput("authors", label = "Authors",
                value = "Marwala T.,Haas Martin,Evans A."),
      
      # Input: Filter by subject area
      checkboxGroupInput("filter", label = "Filter by subject area",
                         choices = list("Choice 1" = 1, "Choice 2" = 2),
                         selected = 1),
      
      # Input: Group by
      selectInput("group", label = "Select color idiom",
                  choices = list("E-mail domain" = 1, "Subject area" = 2,
                                "Affilitation" = 3),
                  selected = 1),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "neighbors",
                  label = "Number of neighbors:",
                  min = 1,
                  max = 10,
                  value = 2)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      forceNetworkOutput(outputId = "net")
      
    )
  )
)

shinyApp(ui = ui, server = server)