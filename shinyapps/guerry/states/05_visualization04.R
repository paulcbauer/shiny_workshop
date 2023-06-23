library(shiny)
library(htmltools)
library(bs4Dash)
library(fresh)
library(waiter)
library(shinyWidgets)
library(Guerry)
library(sf)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(leaflet)
library(plotly)
library(jsonlite)
library(ggplot2)
library(GGally)
library(datawizard)
library(parameters)
library(performance)
library(ggdark)
library(modelsummary)

# Preparation ----

## Clean ----
guerry <- Guerry::gfrance85 %>%
  st_as_sf() %>%
  as_tibble() %>%
  st_as_sf(crs = 27572) %>%
  mutate(Region = case_match(
    Region,
    "C" ~ "Central",
    "E" ~ "East",
    "N" ~ "North",
    "S" ~ "South",
    "W" ~ "West"
  ))

variables <- guerry %>%
  st_drop_geometry() %>%
  select(where(is.numeric) & !all_of(c("COUNT", "dept", "AVE_ID_GEO"))) %>%
  names()

## Aggregate ----
guerry_region <- guerry %>%
  group_by(Region) %>%
  summarise(across(
  .cols = all_of(variables),
  function(x) {
    if (cur_column() %in% c("Area", "Pop1831")) {
      sum(x)
    } else {
      mean(x)
    }
  }
))

## Read text data ----
txts <- read_json("../app_labels.json", simplifyVector = TRUE)

plotly_buttons <- c(
	"sendDataToCloud", "zoom2d", "select2d", "lasso2d", "autoScale2d",
	"hoverClosestCartesian", "hoverCompareCartesian", "resetScale2d"
)


# UI ----

ui <- dashboardPage(
  title = "The Guerry Dashboard",
  ## Header ----
  header = dashboardHeader(
    tags$style("
      /* remove white space from header */
      .navbar {
        padding-top: 0em;
        padding-bottom: 0em;
        padding-right: 0em;
      }
    "),
    span(style = "display: inline-block; width: 100%;"),
    a(
      class = "logo",
      href = "https://gesis.org/",
      img(src = "../www/gesis-logo.png", style = "height: 1.8em;")
    ),
    title = tagList(
      img(src = "../www/workshop-logo.png", width = 35, height = 35),
      span("The Guerry Dashboard", class = "brand-text")
    ),
    skin = "light",
    sidebarIcon = tags$i(class = "fa fa-bars", style = "color: black;")
  ),
  ## Sidebar ----
  sidebar = dashboardSidebar(
  	id = "sidebar",
  	sidebarMenu(
  		id = "sidebarMenu",
  		menuItem(tabName = "intro", text = "Introduction", icon = icon("home")),
  		menuItem(tabName = "insp", text = "Inspect data", icon = icon("table")),
  		menuItem(tabName = "model", text = "Model data", icon = icon("chart-line"), selected = TRUE),
  		flat = TRUE
  	),
  	minified = TRUE,
  	collapsed = TRUE,
  	fixed = FALSE,
  	skin = "light"
  ),
  ## Body ----
  body = dashboardBody(
    tabItems(
    	tabItem("intro"),
    	tabItem("insp"),
    	tabItem(
    		"model",
    		fluidRow(
    			column(
    				width = 6,
    				#### Box: Select variables ----
    				box(
    					width = 12,
    					title = "Select variables",
    					status = "primary",
    					selectInput(
    						"model_x",
    						label = "Select a dependent variable",
    						choices = setNames(variables, sapply(txts, "[[", "title")),
    						selected = "Literacy"
    					),
    					selectizeInput(
    						"model_y",
    						label = "Select independent variables",
    						choices = setNames(variables, sapply(txts, "[[", "title")),
    						multiple = TRUE,
    						selected = "Commerce"
    					),
    					checkboxInput(
    						"model_std",
    						label = "Standardize variables?",
    						value = TRUE
    					),
    					hr(),
    					actionButton(
    						"refresh",
    						label = "Apply changes",
    						icon = icon("refresh"),
    						flat = TRUE
    					)
    				),
    				#### Box: Coefficient/Scatterplot ----
    				tabBox(
    					status = "primary",
    					type = "tabs",
    					width = 12,
    					##### Tab: Coefficient plot ----
    					tabPanel(
    						title = "Plot: Coefficients",
    						plotly::plotlyOutput("coefficientplot")
    					),
    					##### Tab: Scatterplot ----
    					tabPanel(
    						title = "Plot: Scatterplot",
    						plotly::plotlyOutput("scatterplot")
    					),
    					##### Tab: Table: Regression ----
    					tabPanel(
    						title = "Table: Model",
    						htmlOutput("tableregression")
    					)
    				)
    			),
    			#### Box: Pair diagramm ----
    			column(
    				width = 6,
    				box(
    					width = 12,
    					title = "Pair diagram",
    					status = "primary",
    					plotly::plotlyOutput("pairplot")
    				)
    			)
    		)
    	)
    ) # end tabItems
  ),
  ## Controlbar (top)----
  controlbar = dashboardControlbar(
    div(class = "p-3", skinSelector()),
  	skin = "light"
  ),
  ## Footer (bottom)----
  footer = dashboardFooter(
  	left = span(
  		"This dashboard was created by Jonas Lieth and Paul Bauer. Find the source code",
  		a("here.", href = "https://github.com/paulcbauer/shiny_workshop/tree/main/shinyapps/guerry"),
  		"It is based on data from the",
  		a("Guerry R package.", href = "https://cran.r-project.org/web/packages/Guerry/index.html")
  	)
  )
)



# Server ----

server <- function(input, output, session) {
	mparams <- reactive({
		x <- input$model_x
		y <- input$model_y
		dt <- sf::st_drop_geometry(guerry)[c(x, y)]
		if (input$model_std) dt <- datawizard::standardise(dt)
		form <- as.formula(paste(x, "~", paste(y, collapse = " + ")))
		mod <- lm(form, data = dt)
		
		list(x = x,	y = y, data = dt,	model = mod)
	}) %>%
		bindEvent(input$refresh, ignoreNULL = FALSE)
	
	### Pair diagram ----
	output$pairplot <- renderPlotly({
		p <- GGally::ggpairs(mparams()$data, axisLabels = "none")
		ggplotly(p)
	})
	
	### Plot: Coefficientplot ----
	output$coefficientplot <- renderPlotly({
		params <- mparams()
		dt <- params$data
		x <- params$x
		y <- params$y
		
		p <- plot(parameters::model_parameters(params$model))
		
		ggplotly(p)
	})
	
	### Plot: Scatterplot ----
	output$scatterplot <- renderPlotly({
		params <- mparams()
		dt <- params$data
		dt_labels <- params$data_labels
		x <- params$x 
		y <- params$y
		
		
		if (length(y) == 1) {
			p <- ggplot(params$data, 
									aes(x = .data[[params$x]], 
											y = .data[[params$y]])) +
				geom_point() +
				geom_smooth() + 
				theme_light()
		} else {
			p <- ggplot() +
				theme_void() +
				annotate("text", 
								 label = "Cannot create scatterplot.\nMore than two variables selected.", 
								 x = 0, y = 0, 
								 size = 5, 
								 colour = "red",
								 hjust = 0.5,
								 vjust = 0.5) +
				xlab(NULL)
			
		}
		
		ggplotly(p)
	})
	
	### Table: Regression ----
	output$tableregression <- renderUI({
		params <- mparams()
		HTML(modelsummary(
			dvnames(list(params$model)),
			gof_omit = "AIC|BIC|Log|Adj|RMSE"
		))
	})
}

shinyApp(ui, server)