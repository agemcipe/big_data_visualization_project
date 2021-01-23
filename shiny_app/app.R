library(shiny)
library(tidyverse)
library(igraph)
library(networkD3)
library(shinydashboard)
source("helpers.R")


on_node_click <- 'Shiny.setInputValue("clicked_node_name", d.name, {priority: "event"});'



#### Define server logic required to draw output
server <- function(input, output) {

  # Filter by author and select neighbors
  selected_author_df <- reactive({
    if (length(input$authors) == 0) {
      return(full_author_df)
    }
    filter_by_author_and_neighbors(full_author_df, input$authors, input$neighbors, full_graph)
  })

  subgraph_author_df <- reactive({
    if (input$full_subgraph) {
      authors_in_same_comp <- full_author_df[
        full_author_df$membership %in% selected_author_df()$membership,
      ]
    }
    else {
      selected_author_df()
    }
  })


  filtered_by_subject_author_df <- reactive({
    if (length(input$subject_filter) == 0) {
      return(subgraph_author_df())
    }
    filtered_author_df <- subgraph_author_df() %>% filter(
      category_main %in% input$subject_filter
    )
  })

  filtered_author_df <- reactive({
    filter_out_small_connected_components(
      full_network_df,
      filtered_by_subject_author_df(),
      input$min_comp
    )
  })

  links <- reactive({
    links_and_nodes <- prepare_final_links(full_network_df, filtered_author_df())
    links_and_nodes$links
  })
  
  # ColourScale <- reactive({
  #   if(input$group == "selected"){
  #     'd3.scaleOrdinal()
  #           .domain(["Selected", "Unselected"])
  #          .range(["#FF6900", "#694489"]);'
  #   }
  #   else {}
  # })

  output$net <- renderForceNetwork(forceNetwork(
    Links = as.data.frame(links()),
    Nodes = as.data.frame(filtered_author_df()),
    Source = "idx.x",
    Target = "idx.y",
    Value = "cnt_publications",
    NodeID = "author_name",
    fontSize = 20,
    Group = input$group,
    Nodesize = "total_cnt_publications",
    radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
    linkDistance = JS("function(d){return 100/(d.value)}"),
    linkWidth = JS("function(d) { return Math.pow(d.value, 2); }"),
    clickAction = on_node_click,
    zoom = TRUE,
    legend = TRUE# ,
    # colourScale = JS(ColourScale())
  ))

  number_of_nodes <- reactive({
    length(filtered_author_df())
  })

  output$selected_author_chart <- renderPlot(
    make_author_terms_count_plot(input$authors)
  )
  output$clicked_node_chart <- renderPlot(
    make_author_terms_count_plot(input$clicked_node_name)
  )

  output$text <- renderText({
    var <- paste(
      "Your current options are: \n",
      "Authors: ", input$authors, "\n",
      "Number of neighbors: ", input$neighbors, "\n",
      "Subject filter: ", input$subject_filter, "\n",
      "Group by: ", input$group
    )

    if (number_of_nodes() > 100) {
      paste("Graph too large. Please choose less than 10,000 nodes. \n", var)
    }
    else if (number_of_nodes() == 0) {
      paste("No nodes selected.\n", var)
    }
    else {
      paste("You have selected ", number_of_nodes(), " nodes.\n", var)
    }
  })
}

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard"),

  # Sidebar panel for inputs ----
  dashboardSidebar(
    h3("Apply filters"),
    selectizeInput("authors",
                   label = "Select Authors",
                   choices = full_author_df$author_name,
                   multiple = TRUE,
                   options = list(
                     placeholder = "Ashimov S. M.",
                     onInitialize = I('function() {this.setValue("Ashimov S. M.");}')
                   )
    ),
    
    # Input: Slider for the number of neighbors ----
    sliderInput(
      inputId = "neighbors",
      label = "Include neighbors of max degree:",
      min = 1,
      max = 10,
      value = 2
    ),
    
    # Input: Checkbox whether to include full subgraph
    checkboxInput(
      inputId = "full_subgraph",
      label = "Include full subgraphs containing selected authors",
      value = FALSE
    ),
    
    # Input: Filter by subject area
    selectizeInput(
      inputId = "subject_filter",
      label = "Filter by subject area",
      choices = full_author_df$category_main,
      multiple = TRUE,
      options = list(items = full_author_df$category_main)
    ),
    
    # Input: Filter  small connected components  ----
    sliderInput(
      inputId = "min_comp",
      label = "Minimal size of connected graph components:",
      min = 1,
      max = 10,
      value = 1
    ),
    
    h3("Change aesthetics"),
    
    # Input: Group by
    
    selectInput("group",
                label = "Select color idiom",
                choices = list(
                  "Selected Authors" = "selected",
                  "Subject area" = "category_main",
                  "E-mail domain" = "email_domain_end"
                ),
                selected = "selected"
    )
  ),
  
  # Main panel for displaying outputs ----
  dashboardBody(
    verticalLayout(
      h1("Acadamic Co-Authoring"),
      h2("Visualizing the inherent network of the Arxiv Dataset"),
      forceNetworkOutput(outputId = "net"),
      verbatimTextOutput("text"),
      
      splitLayout(
        plotOutput("selected_author_chart"),
        plotOutput("clicked_node_chart")
      )
    )
  )
)



shinyApp(ui = ui, server = server)