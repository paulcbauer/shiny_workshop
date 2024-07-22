library(dplyr)
library(tidyr)
library(shiny)
library(leaflet)
library(haven)

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
    ),
    
    ## Main panel ----
    mainPanel(leafletOutput("map", height = 600))
  )
)


# Server ----
server <- function(input, output, session) {
  # render map ----
  output$map <- renderLeaflet({
    var <- isolate(input$xvar)
    ess_geo <- ess_geo[c("country", var)]
    
    # create a palette for numerics and ordinals
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
        )
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
  
  
  # map proxy
  observe({
    var <- input$xvar
    ess_geo <- ess_geo[c("country", var)]
    pal <- colorNumeric("YlOrRd", domain = NULL)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = ess_geo,
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
        )
      ) %>%
      # add a legend
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ess_geo[[var]],
        opacity = 0.7,
        title = var
      )
  }) %>%
    bindEvent(input$xvar)
  
  
  observe({
    coords <- input$map_shape_click
    leafletProxy("map") %>%
      addCircleMarkers(
        lng = coords$lng,
        lat = coords$lat,
        radius = 1,
        color = "blue",
        fillColor = "blue",
        opacity = 1,
        fillOpacity = 1
      )
  }) %>%
    bindEvent(input$map_shape_click)
}

shinyApp(ui = ui, server = server)
