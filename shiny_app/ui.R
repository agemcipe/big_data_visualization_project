library(shiny)
library(networkD3)

fluidPage(
    # App title ----
    titlePanel("Hello Shiny!"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Author
            textInput("author",
                label = "Author",
                value = "Marwala T."
            ),
            # Input: Filter by subject area
            checkboxGroupInput("filter",
                label = "Filter by subject area",
                choices = list("Choice 1" = 1, "Choice 2" = 2),
                selected = 1
            ),
            # Input: Group by
            selectInput("group",
                label = "Select color idiom",
                choices = list(
                    "E-mail domain" = 1, "Subject area" = 2,
                    "Affilitation" = 3
                ),
                selected = 1
            ),
            # Input: Slider for the number of bins ----
            sliderInput(
                inputId = "neighbors",
                label = "Number of neighbors:",
                min = 1,
                max = 10,
                value = 5
            ),
            textOutput("clicked_node"),
        ),
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Histogram ----
            verticalLayout(
                forceNetworkOutput(outputId = "force"),
                splitLayout(
                    plotOutput("clicked_node_chart"),
                    plotOutput("clicked_node_chart2")
                )
            )
        )
    )
)