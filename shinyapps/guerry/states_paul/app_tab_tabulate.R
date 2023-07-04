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



# 3 UI ----

ui <- dashboardPage(
  title = "The Guerry Dashboard",

  ## 3.1 Header ----
  header = dashboardHeader(
    title = tagList(
      span("The Guerry Dashboard", class = "brand-text")
    )
  ),
  ## 3.2 Sidebar ----
  sidebar = dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(tabName = "tab_tabulate", text = "Tabulate data", icon = icon("table")),
      flat = TRUE
    ),
    minified = TRUE,
    collapsed = TRUE,
    fixed = FALSE,
    skin = "light"
  ),
  ## 3.3 Body ----
  body = dashboardBody(
    tabItems(
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
      )
    ) # end tabItems
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
      data_table <- data_table[, c("Region", "Department",var)]
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
  
  

}

shinyApp(ui, server)