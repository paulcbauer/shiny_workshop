animals <- c("dog", "cat", "mouse") # Predefining some categories

ui <- fluidPage(
  
  # Free text
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  
  # Numeric inputs
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
  
  # Dates
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?"),
  
  # Limited choices
  selectInput("state", "What's your favourite animal?", animals),
              radioButtons("animal", "What's your favourite animal?", animals),
              selectInput( "state", "What's your favourite animal?", animals, multiple = TRUE),
              checkboxGroupInput("animal", "What animals do you like?", animals),
              
              # Single checkbox
              checkboxInput("cleanup", "Clean up?", value = TRUE),
              checkboxInput("shutdown", "Shutdown?"),
              
              # File uploads
              fileInput("upload", NULL),
              
              # Action buttons
              actionButton("click", "Click me!"),
              actionButton("drink", "Drink me!", icon = icon("cocktail"))
  )
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)