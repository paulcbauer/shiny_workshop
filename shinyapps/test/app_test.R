library(shiny)

# Define UI
ui <- fluidPage(
  actionButton("genButton", "Generate Random Number"),
  actionButton("dispButton", "Display Random Number"),
  textOutput("randomNumber")
)

# Define server logic
server <- function(input, output, session) {
  randNum <- reactiveValues(num = NULL) # Create object to store reactiv values
  
  observeEvent(input$genButton, {
    randNum$num <- runif(1) # Generate a random number when genButton is clicked
  })
  
  observeEvent(input$dispButton, {
    output$randomNumber <- renderText({ 
      isolate(randNum$num) # Display the random number when dispButton is clicked, but do not reactivity link it
    }) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(display.mode='showcase'))
