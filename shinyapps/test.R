library(shinydashboard)
# UI ----
ui <- dashboardPage(title = "My Shiny App",
                    
                    ### Header ----
                    header = dashboardHeader(),
                    
                    ### Sidebar ----
                    sidebar = dashboardSidebar(),
                    
                    ### Body ----
                    body = dashboardBody(
                      h2("A NEW HOPE", align = "center"),
                      h5("It is a period of civil war.", align = "center"),
                      p("p creates a paragraph of text."),
                      tags$p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                      strong("strong() makes bold text."),
                      em("em() creates italicized (i.e, emphasized) text."),
                      tags$hr(style="border-color:black;"),
                      tags$br(),
                      tags$line(),
                      br(),
                      code("code displays your text similar to computer code"),
                      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                      br(),
                      p("span does the same thing as div, but it works with",
                        span("groups of words", style = "color:blue"),
                        "that appear inside a paragraph."))
)


# Server ----

server <- function(input, output, session) {}

shinyApp(ui, server)