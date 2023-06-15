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
			img(src = "../gesis-logo.png", style = "height: 1.8em;")
		),
		title = tagList(
			img(src = "../workshop-logo.png", width = 35, height = 35),
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
			menuItem(tabName = "exp", text = "Explore data", icon = icon("map")),
			menuItem(tabName = "insp", text = "Inspect data", icon = icon("table")),
			menuItem(tabName = "model", text = "Model data", icon = icon("chart-line")),
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
			waiter::use_waiter()
		),
		tabItems(
			### Home tab ----
			tabItem(
				tabName = "intro",
				jumbotron(
					title = "The Guerry Dashboard",
					lead = "A Shiny app to explore the classic Guerry dataset.",
					status = "info",
					btnName = NULL
				),
				fluidRow(
					column(width = 1),
					column(
						width = 6,
						box(
							title = "About",
							status = "primary",
							width = 12,
							blockQuote(HTML("André-Michel Guerry was a French lawyer and
                          amateur statistician. Together with Adolphe
                          Quetelet he may be regarded as the founder of
                          moral statistics which led to the development
                          of criminology, sociology and ultimately,
                          modern social science. <br>— Wikipedia: <a href='https://en.wikipedia.org/wiki/Andr%C3%A9-Michel_Guerry'>André-Michel Guerry</a>"),
												 color = "primary"),
							p(HTML("Andre-Michel Guerry (1833) was the first to 
              systematically collect and analyze social data 
               on such things as crime, literacy and suicide 
               with the view to determining social laws and the 
               relations among these variables. The Guerry data 
               frame comprises a collection of 'moral variables' 
               (cf. <i><a href='https://en.wikipedia.org/wiki/Moral_statistics'>moral statistics</a></i>) 
               on the 86 departments of France around 1830. 
               A few additional variables have been added 
               from other sources. In total the data frame has 
               86 observations (the departments of France) on 23 variables <i>(Source: <code>?Guerry</code>)</i>. 
               In this app, we aim to explore Guerry’s data
              	using spatial exploration and regression modelling.")),
							hr(),
							accordion(
								id = "accord",
								accordionItem(
									title = "References",
									status = "primary",
									solidHeader = FALSE,
									"The following sources are referenced in this app:",
									tags$ul(
										class = "list-style: none",
										style = "margin-left: -30px;",
										p("Angeville, A. (1836). Essai sur la Statistique de la Population française Paris: F. Doufour."),
										p("Guerry, A.-M. (1833). Essai sur la statistique morale de la France Paris: Crochard. English translation: Hugh P. Whitt and Victor W. Reinking, Lewiston, N.Y. : Edwin Mellen Press, 2002."),
										p("Parent-Duchatelet, A. (1836). De la prostitution dans la ville de Paris, 3rd ed, 1857, p. 32, 36"),
										p("Palsky, G. (2008). Connections and exchanges in European thematic cartography. The case of 19th century choropleth maps. Belgeo 3-4, 413-426.")
									)
								),
								accordionItem(
									title = "Details",
									status = "primary",
									solidHeader = FALSE,
									p("This app was created as part of a Shiny workshop held in July 2023"),
									p("Last update: June 2023"),
									p("Further information about the data can be found",
										a("here.", href = "https://www.datavis.ca/gallery/guerry/guerrydat.html"))
								)
							)
						)
					),
					column(
						width = 4,
						box(
							title = "André Michel Guerry",
							status = "primary",
							width = 12,
							tags$img(src = "guerry.jpg", width = "100%"),
							p("Source: Palsky (2008)")
						)
					)
				)
			),
			### Explore data ----
			tabItem(
				tabName = "exp", # must correspond to related menuItem name
				fluidRow(
					column(
						width = 4, # must be between 1 and 12
						box(
							title = "Data selection",
							status = "primary",
							width = 12,
							shinyWidgets::pickerInput(
								"exp_select",
								label = "Select a variable",
								choices = setNames(variables, sapply(txts, "[[", "title")),
								options = shinyWidgets::pickerOptions(liveSearch = TRUE)
							),
							uiOutput("exp_desc")
						),
						box(
							title = "Map configuration",
							status = "primary",
							width = 12,
							shinyWidgets::radioGroupButtons(
								"exp_aggr",
								label = "Aggregation level",
								choices = c("Departments", "Regions"),
								selected = "Departments",
								individual = TRUE,
								checkIcon = list(
									yes = tags$i(class = "fa fa-circle", style = "color: #58748f;"),
									no = tags$i(class = "fa fa-circle-o", style = "color: #58748f;")
								)
							),
							shinyWidgets::pickerInput(
								"exp_pal",
								label = "Color palette",
								choices = pals,
								selected = "Reds"
							) # end input
						) # end box
					), # end column
					column(
						width = 8,
						box(
							id = "exp_box",
							status = "primary",
							headerBorder = FALSE,
							collapsible = FALSE,
							width = 12,
							leaflet::leafletOutput("exp_map", height = "800px", width = "100%")
						) # end box
					) # end column
				) # end fluidRow
			), # end tabItem
			### Inspect data ----
			tabItem(
				tabName = "insp",
				fluidRow(
					pickerInput(
						"insp_select",
						label = "Filter variables",
						choices = setNames(variables, sapply(txts, "[[", "title")),
						options = pickerOptions(
							actionsBox = TRUE,
							windowPadding = c(30, 0, 0, 0),
							liveSearch = TRUE,
							selectedTextFormat = "count",
							countSelectedText = "{0} variables selected",
							noneSelectedText = "No filters applied"
						),
						inline = TRUE,
						multiple = TRUE
					),
					div(style = "width: 20px;"),
					radioGroupButtons(
						"insp_aggr",
						label = "Aggregation level",
						choices = c("Departments", "Regions"),
						selected = "Departments",
						individual = TRUE,
						checkIcon = list(
							yes = tags$i(class = "fa fa-circle", style = "color: #58748f;"),
							no = tags$i(class = "fa fa-circle-o", style = "color: #58748f;")
						)
					)
				),
				hr(),
				#### Data table ----
				DT::dataTableOutput("insp_table")
			),
			### Model data ----
			tabItem(
				tabName = "model",
				fluidRow(
					column(
						width = 6,
						#### Box: Select variables ----
						box(
							width = 12,
							title = "Select variables",
							status = "primary",
							shinyWidgets::pickerInput(
								"model_x",
								label = "Select a dependent variable",
								choices = setNames(variables, sapply(txts, "[[", "title")),
								options = shinyWidgets::pickerOptions(liveSearch = TRUE),
								selected = "Literacy"
							),
							shinyWidgets::pickerInput(
								"model_y",
								label = "Select independent variables",
								choices = setNames(variables, sapply(txts, "[[", "title")),
								options = shinyWidgets::pickerOptions(
									actionsBox = TRUE,
									liveSearch = TRUE,
									selectedTextFormat = "count",
									countSelectedText = "{0} variables selected",
									noneSelectedText = "No variables selected"
								),
								multiple = TRUE,
								selected = "Commerce"
							),
							shinyWidgets::prettyCheckbox(
								"model_std",
								label = "Standardize variables?",
								value = TRUE,
								status = "primary",
								shape = "curve"
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
					column(
						width = 6,
						#### Box: Pair diagramm ----
						box(
							width = 12,
							title = "Pair diagram",
							status = "primary",
							plotly::plotlyOutput("pairplot")
						),
						#### Box: Model diagnostics ----
						tabBox(
							status = "primary",
							type = "tabs",
							title = "Model diagnostics",
							width = 12,
							side = "right",
							tabPanel(
								title = "Normality",
								plotly::plotlyOutput("normality")
							),
							tabPanel(
								title = "Outliers",
								plotly::plotlyOutput("outliers")
							),
							tabPanel(
								title = "Heteroskedasticity",
								plotly::plotlyOutput("heteroskedasticity")
							)
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
	## Explore data ----
	
	# Render description of selected variable
	output$exp_desc <- renderUI({
		HTML(txts[[input$exp_select]]$desc)
	})
	
	# Select polygon based on aggregation level
	poly <- reactive({
		if (identical(input$exp_aggr, "Regions")) {
			guerry_region
		} else {
			guerry
		}
	})
	
	# Select palette based on input
	palette <- reactive({
		pal <- input$exp_pal
		if (pal %in% pals$Viridis) {
			pal <- viridis::viridis_pal(option = tolower(pal))(5)
		}
		pal
	}) %>%
		bindEvent(input$exp_pal)
	
	# Compile parameters for leaflet rendering
	params <- reactive({
		poly <- st_transform(poly(), 4326)
		pal <- palette()
		var <- input$exp_select
		
		values <- as.formula(paste0("~", var))
		pal <- colorNumeric(palette = pal, domain = NULL)
		
		reg <- poly[["Region"]]
		dep <- poly[["Department"]]
		val <- poly[[var]]
		
		if (is.null(dep)) {
			dep <- rep(NA, nrow(poly))
		}
		
		# Create labels that are nicely aligned in a grid
		labels <- mapply(
			function(reg, dep, val) {
				HTML(as.character(tags$table(
					tags$tr(
						style = "line-height: 10px",
						tags$td(tags$b("Region: ")),
						tags$td(reg)
					),
					if (!is.na(dep)) {
						tags$tr(
							style = "line-height: 10px",
							tags$td(tags$b("Department: ")),
							tags$td(dep)
						)
					},
					tags$tr(
						style = "line-height: 10px",
						tags$td(tags$b(paste0(txts[[var]]$lgd, ": "))),
						tags$td(round(val, 2))
					)
				)))
			},
			reg = reg, dep = dep, val = val,
			SIMPLIFY = FALSE,
			USE.NAMES = FALSE
		)
		
		list(
			poly = poly,
			var = var,
			pal = pal,
			values = values,
			labels = labels
		)
	})
	
	# Render leaflet for the first time
	output$exp_map <- leaflet::renderLeaflet({
		# Isolate call to params() to prevent render function to be executed
		# every time params() is invalidated. No dependency is made.
		params <- isolate(params())
		leaflet(data = params$poly) %>%
			addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
			addProviderTiles("OpenTopoMap", group = "OTM") %>%
			addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
			addProviderTiles("GeoportailFrance.orthos", group = "Orthophotos") %>%
			addLayersControl(baseGroups = c("OSM", "OTM",
																			"Stamen Toner", "Orthophotos")) %>%
			setView(lng = 3, lat = 47, zoom = 5) %>%
			addPolygons(
				fillColor = as.formula(paste0("~params$pal(", params$var, ")")),
				fillOpacity = 0.7,
				weight = 1,
				color = "black",
				opacity = 0.5,
				label = params$labels,
				highlightOptions = highlightOptions(
					weight = 2,
					color = "black",
					opacity = 0.5,
					fillOpacity = 1,
					bringToFront = TRUE,
					sendToBack = TRUE
				)
			) %>%
			addLegend(
				position = "bottomright",
				pal = params$pal,
				values = params$values,
				opacity = 0.9,
				title = txts[[params$var]]$lgd,
				labFormat = labelFormat(suffix = txts[[params$var]]$unit)
			)
	})
	
	# Create a leaflet proxy. Proxies update map values without re-rendering the
	# entire map, thus increasing performance.
	observe({
		params <- params()
		leafletProxy("exp_map", data = params$poly) %>%
			clearShapes() %>%
			clearControls() %>%
			addPolygons(
				fillColor = as.formula(paste0("~params$pal(", params$var, ")")),
				fillOpacity = 0.7,
				weight = 1,
				color = "black",
				opacity = 0.5,
				label = params$labels,
				highlightOptions = highlightOptions(
					weight = 2,
					color = "black",
					opacity = 0.5,
					fillOpacity = 1,
					bringToFront = TRUE,
					sendToBack = TRUE
				)
			) %>%
			addLegend(
				position = "bottomright",
				na.label = "No data",
				pal = params$pal,
				values = params$values,
				opacity = 0.9,
				title = txts[[params$var]]$lgd,
				labFormat = labelFormat(suffix = txts[[params$var]]$unit)
			)
	})
	
	observe({
		click <- input$exp_map_shape_click
		req(click) # Cancel execution if click is not "truthy"
		leafletProxy("exp_map") %>% # Create proxy object
			clearMarkers() %>% # Remove ALL markers
			addMarkers(lng = click$lng, lat = click$lat) # add a new marker
	})
}

shinyApp(ui, server)