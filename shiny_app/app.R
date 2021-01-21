library(shiny)
library(tidyverse)
library(igraph)
library(networkD3)

#### For testing
data(MisLinks) 
data(MisNodes)

# Helper functions
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

# Load and prepare full dfs of links and nodes
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


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$example <- renderForceNetwork(forceNetwork(Links = MisLinks, Nodes = MisNodes,
                                                    Source = "source", Target = "target",
                                                    Value = "value", NodeID = "name",
                                                    Group = "group", opacity = 0.8))
  
  
  output$net <- renderForceNetwork(forceNetwork(
    Links = full_network_df,
    Nodes = full_author_df,
    Source = "idx_1.x",
    Target = "idx_1.y",
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
      textInput("author", label = "Author",
                value = "Marwala T."),
      
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
                  value = 5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      forceNetworkOutput(outputId = "example")
      
    )
  )
)

shinyApp(ui = ui, server = server)