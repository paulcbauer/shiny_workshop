library(shiny)
library(plotly)
library(leaflet)

ess <- readRDS("../data/ess_clean.rds")
ess_geo <- readRDS("../data/ess_geo.rds")

# UI ----
ui <- fluidPage(
  titlePanel("European Social Survey - round 10"),
  
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(
      
      ### select a variable ----
      selectInput(
        "variable",
        label = "Select a variable",
        choices = c(
          "Time spent consuming political news" = "nwspol",
          "How often do you use the internet?" = "netusoft",
          "Daily time spent on the internet" = "netustm",
          "Most people can be trusted" = "ppltrst",
          "Most people try to be fair" = "pplfair",
          "Most people try to be helpful" = "pplhlp"
        )
      ),
      
      ### select a country ----
      selectizeInput(
        "countries",
        label = "Filter by country",
        choices = unique(ess$cntry),
        selected = "FR",
        multiple = TRUE
      ),
      
      ### filter values ----
      sliderInput(
        "range",
        label = "Set a value range",
        min = min(ess$nwspol, na.rm = TRUE),
        max = max(ess$nwspol, na.rm = TRUE),
        value = range(ess$nwspol, na.rm = TRUE)
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
server <- function(input, output) {
  # update slider ----
  observe({
    var <- na.omit(ess[[input$variable]])
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
    bindEvent(input$variable)
  
  # filter data ----
  filtered <- reactive({
    var <- input$variable
    range <- input$range
    
    # select variable
    plot_data <- ess[c("idno", "cntry", var)]
    plot_data[[var]] <- as.numeric(plot_data[[var]])
    
    # apply range
    plot_data <- plot_data[
      plot_data[[var]] > range[1] &
      plot_data[[var]] < range[2], 
    ]
    plot_data
  })
  
  # render table ----
  output$table <- renderTable({
    ess[ess$cntry %in% input$countries, ]
  }, height = 400)
  
  # render plot ----
  output$plot <- renderPlotly({
    plot_data <- filtered()
    plot_data[[input$variable]]
    p <- ggplot(plot_data) +
      geom_histogram(aes(x = .data[[input$variable]])) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic()
    plotly::ggplotly(p)
  })
  
  # render map ----
  output$map <- renderLeaflet({
    var <- input$variable
    plot_data <- ess_geo[c("cntry", var)]
    
    # create labels with a bold title and a body
    labels <- sprintf(
      "<strong>%s</strong><br>%s",
      plot_data$cntry,
      plot_data[[var]]
    )
    labels <- lapply(labels, HTML)
    
    # create a palette for numerics and ordinals
    if (is.ordered(plot_data[[var]])) {
      pal <- colorFactor("YlOrRd", domain = NULL)
    } else {
      pal <- colorNumeric("YlOrRd", domain = NULL)
    }
    
    # construct leaflet canvas
    leaflet(plot_data) %>%
      # add base map
      addTiles() %>%
      # add choropleths
      addPolygons(
        fillColor = pal(plot_data[[var]]),
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
        values = plot_data[[var]],
        opacity = 0.7,
        title = var
      )
  })
}

runApp(shinyApp(ui=ui, server = server))