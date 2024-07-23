library(tidyverse)
library(shiny)
library(plotly)
library(haven)

ess <- readRDS("ess_trust.rds")

# UI ----
ui <- fluidPage(
  plotlyOutput("plot", height = 600),
  br(),
  verbatimTextOutput("text", placeholder = TRUE)
)


# Server ----
server <- function(input, output, session) {
  # render plot ----
  output$plot <- renderPlotly({
    xvar <- input$xvar
    yvar <- input$yvar
    plot_data <- ess[ess$country %in% "FR", ] %>%
      drop_na() %>%
      mutate(across(where(is.numeric), .fns = as.ordered))
    
    p <- ggplot(plot_data) +
      aes(x = .data[["left_right"]], y = .data[["trust_eu"]], group = .data[["left_right"]]) +
      geom_violin(fill = "lightblue", show.legend = FALSE) +
      theme_classic()
    plotly::ggplotly(p)
  })
  
  
  output$text <- renderPrint({
    event_data("plotly_hover")
  }) %>%
    bindEvent(event_data("plotly_hover"))
}

shinyApp(ui = ui, server = server)
