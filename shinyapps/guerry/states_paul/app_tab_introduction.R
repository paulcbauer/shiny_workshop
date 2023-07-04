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


# 2 Create theme & preload ----
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
# preloader <- list(
#   html = tagList(spin_6(), "Loading ..."),
#   color = "#B3DDFE"
# )



# 3 UI ----

ui <- dashboardPage(
  title = "The Guerry Dashboard",
  freshTheme = dash_theme,
  #preloader = preloader,
  ## 3.1 Header ----
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
      img(src = "gesis-logo.png", style = "height: 1.8em;")
    ),
    title = tagList(
      img(src = "workshop-logo.png", width = 35, height = 35),
      span("The Guerry Dashboard", class = "brand-text")
    ),
    skin = "light",
    sidebarIcon = tags$i(class = "fa fa-bars", style = "color: black;")
  ),
  ## 3.2 Sidebar ----
  sidebar = dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(tabName = "tab_intro", text = "Introduction", icon = icon("home")),
      menuItem(tabName = "tab_tabulate", text = "Tabulate data", icon = icon("table")),
      menuItem(tabName = "tab_model", text = "Model data", icon = icon("chart-line")),
      menuItem(tabName = "tab_map", text = "Map data", icon = icon("map")),
      flat = TRUE
    ),
    minified = TRUE,
    collapsed = TRUE,
    fixed = FALSE,
    skin = "light"
  ),
  ## 3.3 Body ----
  body = dashboardBody(
    tags$head(
      #waiter::use_waiter(),
      includeCSS("www/styles.css")
    ),
    tabItems(
      ### 3.1.1 Tab: Introduction ----
      tabItem(
        tabName = "tab_intro",
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
      )
    ) # end tabItems
  ),

  ## 3.4 Footer (bottom)----
  footer = dashboardFooter(
  	left = span(
  		"This dashboard was created by Jonas Lieth and Paul Bauer. Find the source code",
  		a("here.", href = "https://github.com/paulcbauer/shiny_workshop/tree/main/shinyapps/guerry"),
  		"It is based on data from the",
  		a("Guerry R package.", href = "https://cran.r-project.org/web/packages/Guerry/index.html")
  	)
  ),
  ## 3.5 Controlbar (top)----
  controlbar = dashboardControlbar(
    div(class = "p-3", skinSelector()),
    skin = "light"
  )  
)



# 4 Server ----

server <- function(input, output, session) {}

shinyApp(ui, server)