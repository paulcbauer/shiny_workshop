library(dplyr)
library(tidyr)
library(shiny)
library(plotly)
library(leaflet)
library(haven)

ess <- readRDS("ess_trust.rds")
ess_geo <- readRDS("ess_trust_geo.rds")

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
        selected = "FR",
        multiple = TRUE
      ),
      
      ### filter values ----
      sliderInput(
        "range",
        label = "Set a value range",
        min = min(ess$trust_parliament, na.rm = TRUE),
        max = max(ess$trust_parliament, na.rm = TRUE),
        value = range(ess$trust_parliament, na.rm = TRUE),
        step = 1
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
          title = "Plot",
          plotlyOutput("plot", height = 600)
        ),
        
        ### Map tab ----
        tabPanel(
          title = "Map",
          leafletOutput("map", height = 600)
        )
      )
    )
  )
)


# Server ----
server <- function(input, output, session) {
  # update slider ----
  observe({
    var <- na.omit(ess[[input$xvar]])
    is_ordered <- is.ordered(var)
    var <- as.numeric(var)
    updateSliderInput(
      inputId = "range",
      min = min(var),
      max = max(var),
      value = range(var),
      step = if (is_ordered) 1
    )
  }) %>%
    bindEvent(input$xvar)
  
  # filter data ----
  filtered <- reactive({
    req(input$countries, cancelOutput = TRUE)
    
    xvar <- input$xvar
    yvar <- input$yvar
    range <- input$range
    
    # select country
    ess <- ess[ess$country %in% input$countries, ]
    
    # select variable
    ess <- ess[c("idno", "country", xvar, yvar)]
    
    # apply range
    ess <- ess[ess[[xvar]] > range[1] & ess[[xvar]] < range[2], ]
    ess
  })
  
  # render table ----
  output$table <- renderTable({
    filtered()
  }, height = 400)
  
  # render plot ----
  output$plot <- renderPlotly({
    xvar <- input$xvar
    yvar <- input$yvar
    plot_data <- filtered() %>%
      drop_na() %>%
      mutate(across(where(is.numeric), .fns = as.ordered))
    
    p <- ggplot(plot_data) +
      aes(x = .data[[xvar]], y = .data[[yvar]], group = .data[[xvar]]) +
      geom_violin(fill = "lightblue", show.legend = FALSE) +
      theme_classic()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
