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

# 1 Data preparation ----

## Load & clean data ----
variable_names <- list(
  Crime_pers = "Crime against persons",  
  Crime_prop =  "Crime against property",  
  Literacy = "Literacy",  
  Donations = "Donations to the poor",  
  Infants = "Illegitimate births",  
  Suicides = "Suicides",  
  Wealth = "Tax / capita",  
  Commerce = "Commerce & Industry",  
  Clergy = "Clergy",  
  Crime_parents = "Crime against parents",  
  Infanticide = "Infanticides",  
  Donation_clergy = "Donations to the clergy",  
  Lottery = "Wager on Royal Lottery",  
  Desertion = "Military desertion",  
  Instruction = "Instruction",  
  Prostitutes = "Prostitutes",  
  Distance = "Distance to paris",  
  Area = "Area",  
  Pop1831 = "Population"
)

variable_desc <- list(
  Crime_pers = list(
    title = "Crime against persons",
    desc = as.character(p(tags$b("Crime against persons:"), "Population per crime against persons", hr(), helpText("Source: Table A2 in Guerry (1833). Compte général, 1825-1830"))),
    lgd = "Pop. per crime",
    unit = ""
  ),
  Crime_prop = list(
    title = "Crime against property",
    desc = as.character(p(tags$b("Crime against property:"), "Population per crime against property", hr(), helpText("Source: Compte général, 1825-1830"))),
    lgd = "Pop. per crime",
    unit = ""
  ),
  Literacy = list(
    title = "Literacy",
    desc = as.character(p(tags$b("Percent Read & Write:"), "Percent of military conscripts who can read and write", hr(), helpText("Source: Table A2 in Guerry (1833)"))),
    lgd = "Literacy",
    unit = " %"
  ),
  Donations = list(
    title = "Donations to the poor",
    desc = as.character(p(tags$b("Donations to the poor"), hr(), helpText("Source: Table A2 in Guerry (1833). Bulletin des lois"))),
    lgd = "Donations",
    unit = ""
  ),
  Infants = list(
    title = "Illegitimate births",
    desc = as.character(p(tags$b("Population per illegitimate birth"), hr(), helpText("Source: Table A2 in Guerry (1833). Bureau des Longitudes, 1817-1821"))),
    lgd = "Pop. per birth",
    unit = ""
  ),
  Suicides = list(
    title = "Suicides",
    desc = as.character(p(tags$b("Population per suicide"), hr(), helpText("Source: Table A2 in Guerry (1833). Compte général, 1827-1830"))),
    lgd = "Pop. per suicide",
    unit = ""
  ),
  Wealth = list(
    title = "Tax / capita",
    desc = as.character(p(tags$b("Per capita tax on personal property:"), "A ranked index based on taxes on personal and movable property per inhabitant", hr(), helpText("Source: Table A1 in Guerry (1833)"))),
    lgd = "Tax / capita",
    unit = ""
  ),
  Commerce = list(
    title = "Commerce & Industry",
    desc = as.character(p(tags$b("Commerce & Industry:"), "Commerce and Industry, measured by the rank of the number of patents / population", hr(), helpText("Source: Table A1 in Guerry (1833)"))),
    lgd = "Patents / capita",
    unit = ""
  ),
  Clergy = list(
    title = "Clergy",
    desc = as.character(p(tags$b("Distribution of clergy:"), "Distribution of clergy, measured by the rank of the number of Catholic priests in active service / population", hr(), helpText("Source: Table A1 in Guerry (1833). Almanach officiel du clergy, 1829"))),
    lgd = "Priests / capita",
    unit = ""
  ),
  Crime_parents = list(
    title = "Crime against parents",
    desc = as.character(p(tags$b("Crime against parents:"), "Crimes against parents, measured by the rank of the ratio of crimes against parents to all crimes \u2013 Average for the years 1825-1830", hr(), helpText("Source: Table A1 in Guerry (1833). Compte général"))),
    lgd = "Share of crimes",
    unit = " %"
  ),
  Infanticide = list(
    title = "Infanticides",
    desc = as.character(p(tags$b("Infanticides per capita:"), "Ranked ratio of number of infanticides to population \u2013 Average for the years 1825-1830", hr(), helpText("Source: Table A1 in Guerry (1833). Compte général"))),
    lgd = "Infanticides / capita",
    unit = ""
  ),
  Donation_clergy = list(
    title = "Donations to the clergy",
    desc = as.character(p(tags$b("Donations to the clergy:"), "Ranked ratios of the number of bequests and donations inter vivios to population \u2013 Average for the years 1815-1824", hr(), helpText("Source: Table A1 in Guerry (1833). Bull. des lois, ordunn. d’autorisation"))),
    lgd = "Donations / capita",
    unit = ""
  ),
  Lottery = list(
    title = "Wager on Royal Lottery",
    desc = as.character(p(tags$b("Per capita wager on Royal Lottery:"), "Ranked ratio of the proceeds bet on the royal lottery to population \u2013 Average for the years 1822-1826", hr(), helpText("Source: Table A1 in Guerry (1833). Compte rendu par le ministre des finances"))),
    lgd = "Wager / capita",
    unit = ""
  ),
  Desertion = list(
    title = "Military desertion",
    desc = as.character(p(tags$b("Military desertion:"), "Military disertion, ratio of the number of young soldiers accused of desertion to the force of the military contingent, minus the deficit produced by the insufficiency of available billets\u2013 Average of the years 1825-1827", hr(), helpText("Source: Table A1 in Guerry (1833). Compte du ministère du guerre, 1829 état V"))),
    lgd = "No. of desertions",
    unit = ""
  ),
  Instruction = list(
    title = "Instruction",
    desc = as.character(p(tags$b("Instruction:"), "Ranks recorded from Guerry's map of Instruction. Note: this is inversely related to literacy (as defined here)")),
    lgd = "Instruction",
    unit = ""
  ),
  Prostitutes = list(
    title = "Prostitutes",
    desc = as.character(p(tags$b("Prostitutes in Paris:"), "Number of prostitutes registered in Paris from 1816 to 1834, classified by the department of their birth", hr(), helpText("Source: Parent-Duchatelet (1836), De la prostitution en Paris"))),
    lgd = "No. of prostitutes",
    unit = ""
  ),
  Distance = list(
    title = "Distance to paris",
    desc = as.character(p(tags$b("Distance to Paris (km):"), "Distance of each department centroid to the centroid of the Seine (Paris)", hr(), helpText("Source: Calculated from department centroids"))),
    lgd = "Distance",
    unit = " km"
  ),
  Area = list(
    title = "Area",
    desc = as.character(p(tags$b("Area (1000 km^2)"), hr(), helpText("Source: Angeville (1836)"))),
    lgd = "Area",
    unit = " km\u00b2"
  ),
  Pop1831 = list(
    title = "Population",
    desc = as.character(p(tags$b("Population in 1831, in 1000s"), hr(), helpText("Source: Taken from Angeville (1836), Essai sur la Statistique de la Population français"))),
    lgd = "Population (in 1000s)",
    unit = ""
  )
)

data_guerry <- Guerry::gfrance85 %>%
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
  )) %>%
  select(-c("COUNT", "dept", "AVE_ID_GEO", "CODE_DEPT")) %>%
  select(Region:Department, all_of(names(variable_names)))



## Prep data (Tab: Tabulate data) ----
data_guerry_tabulate <- data_guerry %>% 
  st_drop_geometry() %>% 
  mutate(across(.cols = all_of(names(variable_names)), round, 2))


## Prep data (Tab: Map data) ----
data_guerry_region <- data_guerry %>%
  group_by(Region) %>%
  summarise(across(
  .cols = all_of(names(variable_names)),
  function(x) {
    if (cur_column() %in% c("Area", "Pop1831")) {
      sum(x)
    } else {
      mean(x)
    }
  }
))

## Prepare palettes ----
## Used for mapping
pals <- list(
  Sequential = RColorBrewer::brewer.pal.info %>%
    filter(category %in% "seq") %>%
    row.names(),
  Viridis = c("Magma", "Inferno", "Plasma", "Viridis",
              "Cividis", "Rocket", "Mako", "Turbo")
)

## Prepare modebar clean-up ----
## Used for modelling
plotly_buttons <- c(
	"sendDataToCloud", "zoom2d", "select2d", "lasso2d", "autoScale2d",
	"hoverClosestCartesian", "hoverCompareCartesian", "resetScale2d"
)

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
preloader <- list(
  html = tagList(spin_6(), "Loading ..."),
  color = "#B3DDFE"
)



# 3 UI ----

ui <- dashboardPage(
  title = "The Guerry Dashboard",
  freshTheme = dash_theme,
  preloader = preloader,
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
      waiter::use_waiter(),
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
      ),
      ### 3.3.2 Tab: Tabulate data ----
      tabItem(
        tabName = "tab_tabulate",
        fluidRow(
          #### Inputs(s) ----
          pickerInput(
            "tab_tabulate_select",
            label = "Filter variables",
            choices = setNames(names(variable_names), variable_names),
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
          )
        ),
        hr(),
        #### Output(s) (Data table) ----
        DT::dataTableOutput("tab_tabulate_table")
      ),
      ### 3.3.3 Tab: Model data ----
      tabItem(
        tabName = "tab_model",
        fluidRow(
          column(
            width = 6,
            #### Inputs(s) ----
            box(
              width = 12,
              title = "Select variables",
              status = "primary",
              shinyWidgets::pickerInput(
                "model_x",
                label = "Select a dependent variable",
                choices = setNames(names(variable_names), variable_names),
                options = shinyWidgets::pickerOptions(liveSearch = TRUE),
                selected = "Literacy"
              ),
              shinyWidgets::pickerInput(
                "model_y",
                label = "Select independent variables",
                choices = setNames(names(variable_names), variable_names),
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
            #### Outputs(s) ----
            tabBox(
              status = "primary",
              type = "tabs",
              title = "Model analysis",
              side = "right",
              width = 12,
              ##### Tabpanel: Coefficient plot ----
              tabPanel(
                title = "Plot: Coefficients",
                plotly::plotlyOutput("coefficientplot")
              ),
              ##### Tabpanel: Scatterplot ----
              tabPanel(
                title = "Plot: Scatterplot",
                plotly::plotlyOutput("scatterplot")
              ),
              ##### Tabpanel: Table: Regression ----
              tabPanel(
                title = "Table: Model",
                htmlOutput("tableregression")
              )
            )
          ),
          column(
            width = 6,
            ##### Box: Pair diagramm ----
            box(
              width = 12,
              title = "Pair diagram",
              status = "primary",
              plotly::plotlyOutput("pairplot")
            ),
            ##### TabBox: Model diagnostics ----
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
      ),
      ### 3.3.4 Tab: Map data ----
      tabItem(
        tabName = "tab_map", # must correspond to related menuItem name
        fluidRow(
          column(
            #### Inputs(s) ----
            width = 4, # must be between 1 and 12
            box(
              title = "Data selection",
              status = "primary",
              width = 12,
              shinyWidgets::pickerInput(
                "tab_map_select",
                label = "Select a variable",
                choices = setNames(names(variable_names), variable_names),
                options = shinyWidgets::pickerOptions(liveSearch = TRUE)
              ),
              uiOutput("tab_map_desc")
            ),
            box(
              title = "Map configuration",
              status = "primary",
              width = 12,
              shinyWidgets::radioGroupButtons(
                "tab_map_aggr",
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
                "tab_map_pal",
                label = "Color palette",
                choices = pals,
                selected = "Reds"
              ) # end input
            ) # end box
          ), # end column
          column(
            #### Output(s) ----
            width = 8,
            box(
              id = "tab_map_box",
              status = "primary",
              headerBorder = FALSE,
              collapsible = FALSE,
              width = 12,
              leaflet::leafletOutput("tab_map_map", height = "800px", width = "100%")
            ) # end box
          ) # end column
        ) # end fluidRow
      ) # end tabItem
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

server <- function(input, output, session) {
  
  ## 4.1 Tabulate data ----
  ### Variable selection ----
  tab <- reactive({
    var <- input$tab_tabulate_select
    data_table <- data_guerry_tabulate
    
    if (!is.null(var)) {
      data_table <- data_table[, var]
    }

    data_table
  })
  

  ### Create table----
  dt <- reactive({
    tab <- tab()
    ridx <- ifelse("Department" %in% names(tab), 3, 1)
    DT::datatable(
      tab,
      class = "hover",
      extensions = c("Buttons"),
      selection = "none",
      filter = list(position = "top", clear = FALSE),
      style = "bootstrap4",
      rownames = FALSE,
      options = list(
        dom = "Brtip",
        deferRender = TRUE,
        scroller = TRUE,
        buttons = list(
          list(extend = "copy", text = "Copy to clipboard"),
          list(extend = "pdf", text = "Save as PDF"),
          list(extend = "csv", text = "Save as CSV"),
          list(extend = "excel", text = "Save as JSON", action = DT::JS("
          function (e, dt, button, config) {
            var data = dt.buttons.exportData();
  
            $.fn.dataTable.fileSave(
              new Blob([JSON.stringify(data)]),
              'Shiny dashboard.json'
            );
          }
        "))
        )
      )
    )
  })
  
  ### Render table----
  output$tab_tabulate_table <- DT::renderDataTable(dt(), server = FALSE)
  
  
  
  ## 4.2 Model data ----
  ### Define & estimate model ----
  mparams <- reactive({
    x <- input$model_x
    y <- input$model_y
    dt <- sf::st_drop_geometry(data_guerry)[c(x, y)]
    dt_labels <- sf::st_drop_geometry(data_guerry)[c("Department", "Region")]
    if (input$model_std) dt <- datawizard::standardise(dt)
    form <- as.formula(paste(x, "~", paste(y, collapse = " + ")))
    mod <- lm(form, data = dt)
    
    list(
      x = x,
      y = y,
      data = dt,
      data_labels = dt_labels,
      model = mod
    )
  }) %>%
    bindEvent(input$refresh, ignoreNULL = FALSE)
  
  ### Pair diagram ----
  output$pairplot <- plotly::renderPlotly({
    params <- mparams()
    dt <- params$data
    dt_labels <- params$data_labels
    p <- GGally::ggpairs(
      params$data,
      axisLabels = "none",
      lower = list(
        continuous = function(data, mapping, ...) {
          ggplot(data, mapping) +
            suppressWarnings(geom_point(
              aes(text = paste0(
                "Department: ", 
                dt_labels[["Department"]],
                "<br>Region: ", 
                dt_labels[["Region"]])),
              color = "black"
            ))
        }
      )
    )
    if (isTRUE(input$dark_mode)) p <- p +
      dark_theme_gray() +
      theme(plot.background = element_rect(fill = "#343a40"))
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = plotly_buttons,
             displaylogo = FALSE)
  })
  
  ### Plot: Coefficientplot ----
  output$coefficientplot <- renderPlotly({
    params <- mparams()
    dt <- params$data
    x <- params$x
    y <- params$y
    
    
    p <- plot(parameters::model_parameters(params$model))
    
    if (isTRUE(input$dark_mode)) p <- p +
      geom_point(color = "white") +
      dark_theme_gray() +
      theme(plot.background = element_rect(fill = "#343a40"))
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = plotly_buttons,
             displaylogo = FALSE)
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
        geom_point(aes(text = paste0("Department: ", 
                                     dt_labels[["Department"]],
                                     "<br>Region: ", 
                                     dt_labels[["Region"]])),
                   color = "black") +
        geom_smooth() + 
        geom_smooth(method='lm') +
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
    
    if (isTRUE(input$dark_mode)) p <- p +
      geom_point(color = "white") +
      dark_theme_gray() +
      theme(plot.background = element_rect(fill = "#343a40"))
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = plotly_buttons,
             displaylogo = FALSE)
  })
  
  ### Table: Regression ----
  output$tableregression <- renderUI({
    params <- mparams()
    HTML(modelsummary(
      dvnames(list(params$model)),
      gof_omit = "AIC|BIC|Log|Adj|RMSE"
    ))
  })
  
  ### Plot: Normality residuals ----
  output$normality <- renderPlotly({
    params <- mparams()
    p <- plot(performance::check_normality(params$model))
    if (isTRUE(input$dark_mode)) p <- p +
      dark_theme_gray() +
      theme(plot.background = element_rect(fill = "#343a40"))
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = plotly_buttons,
             displaylogo = FALSE)
  })
  
  ### Plot: Outliers ----
  output$outliers <- renderPlotly({
    params <- mparams()
    p <- plot(performance::check_outliers(params$model), show_labels = FALSE)
    if (isTRUE(input$dark_mode)) p <- p +
      dark_theme_gray() +
      theme(plot.background = element_rect(fill = "#343a40"))
    p$labels$x <- "Leverage"
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = plotly_buttons,
             displaylogo = FALSE)
  })
  
  ### Plot: Heteroskedasticity ----
  output$heteroskedasticity <- renderPlotly({
    params <- mparams()
    p <- plot(performance::check_heteroskedasticity(params$model))
    if (isTRUE(input$dark_mode)) p <- p +
      dark_theme_gray() +
      theme(plot.background = element_rect(fill = "#343a40"))
    p$labels$y <- "Sqrt. |Std. residuals|" # ggplotly doesn't support expressions
    ggplotly(p) %>%
      config(modeBarButtonsToRemove = plotly_buttons,
             displaylogo = FALSE)
  })
  
  
  ## 4.3 Map data ----
  
  # Render description of selected variable
  output$tab_map_desc <- renderUI({
    HTML(variable_desc[[input$tab_map_select]]$desc)
  })
  
  # Select polygon based on aggregation level
  poly <- reactive({
    if (identical(input$tab_map_aggr, "Regions")) {
      data_guerry_region
    } else {
      data_guerry
    }
  })
  
  # Select palette based on input
  palette <- reactive({
    pal <- input$tab_map_pal
    if (pal %in% pals$Viridis) {
      pal <- viridis::viridis_pal(option = tolower(pal))(5)
    }
    pal
  }) %>%
    bindEvent(input$tab_map_pal)
  
  # Compile parameters for leaflet rendering
  params <- reactive({
    poly <- st_transform(poly(), 4326)
    pal <- palette()
    var <- input$tab_map_select

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
            tags$td(tags$b(paste0(variable_desc[[var]]$lgd, ": "))),
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
  output$tab_map_map <- leaflet::renderLeaflet({
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
        title = variable_desc[[params$var]]$lgd,
        labFormat = labelFormat(suffix = variable_desc[[params$var]]$unit)
      )
  })
  
  # Create a leaflet proxy. Proxies update map values without re-rendering the
  # entire map, thus increasing performance.
  observe({
    params <- params()
    leafletProxy("tab_map_map", data = params$poly) %>%
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
        title = variable_desc[[params$var]]$lgd,
        labFormat = labelFormat(suffix = variable_desc[[params$var]]$unit)
      )
  })
  
  

}

shinyApp(ui, server)