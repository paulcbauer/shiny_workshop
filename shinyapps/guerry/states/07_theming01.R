library(shiny)
library(bslib)

ui <- fluidPage(
	h1("Heading 1"), 
	h2("Heading 2"), 
	p("Paragraph"), 
	selectInput("select", "Select Input", unique(iris$Species)),
	sliderInput("slider", label = "Slider", 1, 5, 3),
	checkboxInput("check", "Checkbox"),
	theme = bs_theme(version = 5, bootswatch = "sketchy")
)

server <- function(input, output, session) {}

shinyApp(ui, server)