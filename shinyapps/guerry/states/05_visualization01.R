library(shiny)
library(htmltools)
library(bs4Dash)
library(shinyWidgets)
library(Guerry)
library(sf)
library(tidyr)
library(dplyr)
library(plotly)
library(jsonlite)
library(GGally)

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
  ## Header ----
  header = dashboardHeader(
    title = tagList(
      img(src = "../www/workshop-logo.png", width = 35, height = 35),
      span("The Guerry Dashboard", class = "brand-text")
    )
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
      ### Model data ----
      tabItem(
      	"model",
      	fluidRow(
      		column(
      			width = 6,
      			box(
      				width = 12,
      				title = "Pair diagram",
      				status = "primary",
      				plotOutput("pairplot")
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
	output$pairplot <- renderPlot({
		dt <- st_drop_geometry(guerry[c("Literacy", "Commerce")])
		GGally::ggpairs(dt, axisLabels = "none")
	})
}

shinyApp(ui, server)