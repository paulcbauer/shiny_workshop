library(shiny)
library(Guerry)
library(tidyverse)
library(sf)


## Data: Clean & prepare data ----
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
  )) %>%
  select(!any_of(c("CODE_DEPT", "COUNT", "AVE_ID_GEO", "dept"))) %>%
  st_drop_geometry() %>%
  select(1:6)


# UI ----

ui <- dashboardPage( # start UI
  title = "The Guerry Dashboard",
  
  ### Header ----
  header = dashboardHeader(
    title = tagList(
      img(src = "workshop-logo.png", width = 35, height = 35),
      span("The Guerry Dashboard", class = "brand-text")
    )
  ),
  
  ### Sidebar ----
  sidebar = dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(tabName = "insp", 
               text = "Table data", 
               icon = icon("table"))
    )
  ),
  ### Body ----
  body = dashboardBody(
    tabItems( # start tabItems
      
      tabItem(
        tabName = "insp",
        fluidRow(
          pickerInput(
            "insp_select",
            label = "Filter variables",
            choices = names(guerry),
            multiple = TRUE
          )
        ),
        hr(),
        #### Data table ----
        DT::dataTableOutput("insp_table")
      )
      
    ) # end tabItems
  )
) # End UI



# Server ----

server <- function(input, output, session) {
  
  tab <- reactive({
    var <- input$insp_select
    data_table <- guerry
    if (!is.null(var)) {data_table <- data_table[var]}
    data_table
  })
  
  
  ## Create table ----
  dt <- reactive({
    
    DT::datatable(
      tab(),
      class = "hover",
      selection = "none",
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE
    )
    
  })
  
  ## Render table ----
  output$insp_table <- DT::renderDataTable(dt(), server = FALSE)
  
}

shinyApp(ui, server)