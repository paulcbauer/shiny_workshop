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

## Pivot ----
guerry_long <- tidyr::pivot_longer(
  st_drop_geometry(guerry),
  cols = all_of(variables),
  names_to = "variable"
) %>%
  mutate(variable = factor(variable, levels = unique(variable))) %>%
  arrange(variable)

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

guerry_region_long <- tidyr::pivot_longer(
  st_drop_geometry(guerry_region),
  cols = all_of(variables),
  names_to = "variable"
) %>%
  mutate(variable = factor(variable, levels = unique(variable))) %>%
  arrange(variable)

## Read text data ----
txts <- read_json("../app_labels.json", simplifyVector = TRUE)

## Prepare palettes ----
pals <- list(
  Sequential = RColorBrewer::brewer.pal.info %>%
    filter(category %in% "seq") %>%
    row.names(),
  Viridis = c("Magma", "Inferno", "Plasma", "Viridis",
              "Cividis", "Rocket", "Mako", "Turbo")
)

plotly_buttons <- c(
	"sendDataToCloud", "zoom2d", "select2d", "lasso2d", "autoScale2d",
	"hoverClosestCartesian", "hoverCompareCartesian", "resetScale2d"
)

## Create theme ----
dash_theme <- create_theme(
  bs4dash_status(
    primary = "#58748f",
    secondary = "#666666",
    info = "#E6EAEE",
    danger = "#BF616A",
    warning = "#FF6100",
    light = "#F4F4F2",
    dark = "#2c2c25"
  ),
  bs4dash_layout(
    font_size_root = "5rem",
    main_bg = "#FDFDFD",
    sidebar_width = "350px"
  ),
  bs4dash_sidebar_light(bg = "#F4F4F2", color = "#000"),
  bs4dash_sidebar_dark(bg = "#2c2c25", color = "#FFF"),
  bs4dash_color(
  	orange = "#F06400",
    white = "#FDFDFD",
    black = "#000",
    gray_600 = "#666",
    gray_800 = "#333",
    gray_900 = "#000",
    blue = "#58748f"
  ),
  bs4dash_font(
    family_sans_serif = "Verdana",
    family_base = "Georgia",
    family_monospace = "Courier New"
  )
)

## Preloader ----
preloader <- list(
  html = tagList(spin_6(), "Loading ..."),
  color = "#B3DDFE"
)



# UI ----

ui <- dashboardPage(
  title = "The Guerry Dashboard",
  freshTheme = dash_theme,
  preloader = preloader,
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
    tags$head(
      waiter::use_waiter(),
      includeCSS("../www/styles.css")
    ),
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
	dt <- reactive({
		x <- input$model_x
		y <- input$model_y
		dt <- sf::st_drop_geometry(guerry)[c(x, y)]
		if (input$model_std) dt <- datawizard::standardise(dt)
		dt
	}) %>%
		bindEvent(input$refresh, ignoreNULL = FALSE)
	
	output$pairplot <- plotly::renderPlotly({
		p <- GGally::ggpairs(dt(), axisLabels = "none")
		plotly::ggplotly(p) %>%
			config(
				modeBarButtonsToRemove = c(
					"sendDataToCloud", "zoom2d", "select2d", "lasso2d", "autoScale2d",
					"hoverClosestCartesian", "hoverCompareCartesian", "resetScale2d"
				),
				displaylogo = FALSE,
				toImageButtonOptions = list(
					format = "svg",
					filename = "guerry_plot",
					height = NULL, # adopt height and width as is
					width = NULL
				),
				scrollZoom = TRUE
			)
	})
}

shinyApp(ui, server)