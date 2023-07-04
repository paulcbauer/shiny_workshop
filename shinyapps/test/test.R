library(shiny)

# Define UI
ui <- fluidPage(
  actionButton("genButton", "Generate Random Number"),
  actionButton("dispButton", "Display Random Number"),
  textOutput("randomNumber")
)

# Define server logic
server <- function(input, output) {
  randNum <- reactiveValues(num = NULL)
  
  observeEvent(input$genButton, {
    randNum$num <- runif(1) # Generate a random number when genButton is clicked
  })
  
  output$randomNumber <- renderText({ 
    randNum$num # Generate the reactive expression
  }) %>% 
    bindEvent(input$dispButton) # Binding the output$randomNumber reactive expression to dispButton
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(display.mode='showcase'))