library(tidyverse)
library(shiny)
library(plotly) # needed to render interactive plots
library(leaflet) # needed to render interactive maps
library(haven) # needed to display ess labels
library(sf) # needed to display and process map data

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
    req(input$countries)
    
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
      aes(x = .data[[yvar]], y = .data[[xvar]], group = .data[[yvar]]) +
      geom_violin(fill = "lightblue", show.legend = FALSE) +
      theme_classic()
    plotly::ggplotly(p)
  })
  
  # render map ----
  output$map <- renderLeaflet({
    var <- input$xvar
    ess_geo <- ess_geo[c("country", var)]
    
    # create labels with a bold title and a body
    labels <- sprintf(
      "<strong>%s</strong><br>%s",
      ess_geo$country,
      ess_geo[[var]]
    )
    labels <- lapply(labels, HTML)
    
    # create a palette for numerics
    pal <- colorNumeric("YlOrRd", domain = NULL)

    # construct leaflet canvas
    leaflet(ess_geo) %>%
      # add base map
      addTiles() %>%
      # add choropleths
      addPolygons(
        fillColor = pal(ess_geo[[var]]),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        # highlight polygons on hover
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels
      ) %>%
      # add a legend
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ess_geo[[var]],
        opacity = 0.7,
        title = var
      )
  })
}

shinyApp(ui = ui, server = server)
