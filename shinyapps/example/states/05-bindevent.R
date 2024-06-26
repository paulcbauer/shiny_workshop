library(dplyr)
library(tidyr)
library(shiny)
library(plotly)
library(leaflet)
library(haven)

ess <- readRDS("../../../data/ess_trust.rds")
ess_geo <- readRDS("../../../data/ess_trust_geo.rds")

# UI ----
ui <- fluidPage(
  titlePanel("European Social Survey - round 10"),
  
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(
      ### select dependent variable
      selectInput(
        "xvar",
        label = "Select a dependent variable",
        choices = c(
          "Trust in country's parliament" = "trust_parliament",
          "Trust in the legal system" = "trust_legal",
          "Trust in the police" = "trust_police",
          "Trust in politicians" = "trust_politicians",
          "Trust in political parties" = "trust_parties",
          "Trust in the European Parliament" = "trust_eu",
          "Trust in the United Nations" = "trust_un"
        )
      ),
      
      ### select a variable ----
      selectInput(
        "yvar",
        label = "Select an independent variable",
        choices = c(
          "Placement on the left-right scale" = "left_right",
          "Age" = "age",
          "Feeling about household's income" = "income_feeling",
          "How often do you use the internet?" = "internet_use",
          "How happy are you?" = "happiness"
        )
      ),
      
      ### select a country ----
      selectizeInput(
        "countries",
        label = "Filter by country",
        choices = unique(ess$country),
        multiple = TRUE
      ),
      
      actionButton( # <1> 
        "button", # <1>
        label = "Update parameters", # <1>
        icon = icon("refresh") # <1>
      )
    ),
    
    ## Main panel ----
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        ### Table tab ----
        tabPanel(
          title = "Table",
          div(
            style = "height: 600px; overflow-y: auto;",
            tableOutput("table")
          )
        ),
        
        ### Plot tab ----
        tabPanel(
          title = "Histogram",
          plotlyOutput("plot", height = 600)
        )
      )
    )
  )
)


# Server ----
server <- function(input, output) {
  # filter data ----
  filtered <- reactive({
    xvar <- input$xvar
    yvar <- input$yvar
    range <- input$range
    
    # select country
    if (!is.null(input$countries)) {
      ess <- ess[ess$country %in% input$countries, ]
    }
    
    # select variable
    ess[c("idno", "country", xvar, yvar)]
  }) %>% # <1>
    bindEvent(input$button, ignoreNULL = FALSE)
  
  # render table ----
  output$table <- renderTable({
    ess[ess$country %in% input$countries, ]
  }, height = 400)
  
  # render plot ----
  output$plot <- renderPlotly({
    req(input$button)
    xvar <- input$xvar
    yvar <- input$yvar
    plot_data <- filtered() %>%
      drop_na() %>%
      mutate(across(is.numeric, .fns = as.ordered))
    
    ggplot(plot_data) +
      aes(x = .data[[xvar]], y = .data[[yvar]], group = .data[[xvar]]) +
      geom_violin(fill = "lightblue", show.legend = FALSE) +
      theme_classic()
  })
  
  # executes everytime `filtered()` is updated
  # prints the filtered dataset to the console
  observe({
    print(filtered())
  })
}

shinyApp(ui = ui, server = server)