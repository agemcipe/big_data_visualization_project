library(shiny)
library(tidyverse)
library(igraph)
library(networkD3)
source("helpers.R")


#### Define server logic required to draw output
server <- function(input, output) {
  
  # Filter by author and select neighbors
  selected_author_df <- reactive({
    # filter_by_author_and_neighbors(full_author_df, authors()[[1]],input$neighbors, full_graph) # "Marwala T.,Haas Martin"
    if(length(input$authors) == 0) {return(full_author_df)}
    filter_by_author_and_neighbors(full_author_df, input$authors,input$neighbors, full_graph) # "Marwala T.,Haas Martin"
  })
  
  subgraph_author_df <- reactive({
    if(input$full_subgraph){
      authors_in_same_comp <- full_author_df[
        full_author_df$membership %in% selected_author_df()$membership,
      ]  
    }
    else {
      selected_author_df()
    }
  })

  
  filtered_by_subject_author_df <- reactive({
    if(length(input$subject_filter) == 0) {return(subgraph_author_df())}
    filtered_author_df <- subgraph_author_df() %>% filter(
      categories_first %in% input$subject_filter
    )
  })
  
  filtered_author_df <- reactive({
    filter_out_small_connected_components(full_network_df, 
                                          filtered_by_subject_author_df(),
                                          input$min_comp)
  })
  
  links <- reactive({
    links_and_nodes <- prepare_final_links(full_network_df, filtered_author_df())
    links_and_nodes$links
  })
  
  output$net <- renderForceNetwork(forceNetwork(
    Links = links(),
    Nodes = filtered_author_df(),
    Source = "idx.x",
    Target = "idx.y",
    Value = "cnt_publications",
    NodeID = "author_name",
    Group = input$group,
    # Nodesize = "auth_cnt_publications",
    # radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
    # linkDistance = JS("function(d){return 100/(d.value)}"),
    # linkWidth = JS("function(d) { return Math.pow(d.value, 2); }"),
    zoom = TRUE,
    legend = TRUE,
    # colourScale = JS(ColourScale)
  )
  )
  
  number_of_nodes <- reactive({
    length(filtered_author_df())
  })
  
  output$text <- renderText({
    var <- paste("Your current options are: \n",
                 "Authors: ", input$authors, "\n",
                 "Number of neighbors: ", input$neighbors, "\n",
                 "Subject filter: ", input$subject_filter, "\n",
                 "Group by: ", input$group)
    
    if(number_of_nodes() > 10000) {paste("Graph too large. Please choose less than 10,000 nodes. \n", var)}
    else if (number_of_nodes() == 0) {paste("No nodes selected.\n", var)}
    else {paste("You have selected ", number_of_nodes(), " nodes.\n", var)}
  })
  
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectizeInput("authors", label = "Select Authors", 
                     choices = full_author_df$author_name,
                     multiple = TRUE, 
                     options = list(placeholder = 'Ashimov S. M.',
                                    onInitialize = I('function() {this.setValue("Ashimov S. M.");}'))),
      
      # Input: Slider for the number of neighbors ----
      sliderInput(inputId = "neighbors",
                  label = "Include neighbors of max degree:",
                  min = 1,
                  max = 10,
                  value = 2),
      
      # Input: Checkbox whether to include full subgraph
      checkboxInput(inputId = "full_subgraph",
                    label = "Include full subgraphs containing selected authors",
                    value = FALSE),
      
      # Input: Filter by subject area
      selectizeInput(inputId = "subject_filter", 
                     label = "Filter by subject area", 
                     choices = full_author_df$categories_first,
                     multiple = TRUE),
      
      # Input: Filter  small connected components  ----
      sliderInput(inputId = "min_comp",
                  label = "Minimal size of connected graph components:",
                  min = 1,
                  max = 10,
                  value = 1),
      
      # Input: Group by
      selectInput("group", label = "Select color idiom",
                  choices = list("Subject area" = "categories_first","E-mail domain" = "email_domain", 
                                "Affilitation" = "affiliation"),
                  selected = "categories_first")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      forceNetworkOutput(outputId = "net"),
      verbatimTextOutput('text')
    )
  )
)

shinyApp(ui = ui, server = server)