library(shinydashboard)
# UI ----
# UI ----
# UI ----
ui <- dashboardPage(title = "The Guerry Dashboard",
                    
                    ### Header ----
                    header = dashboardHeader(
                      title = tagList(
                        "image here",
                        span("The Guerry Dashboard", class = "brand-text")
                      )
                    ),
                    
                    ### Sidebar ----
                    sidebar = dashboardSidebar(
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
                          hr(),
                          img(src="http://assets.schwarzenegger.com/images/img-2.jpg", height = 174, width = 300)
                        )
                        
                      ) # end tabItems
                    )
) # End UI



# Server ----

server <- function(input, output, session) {}

shinyApp(ui, server)