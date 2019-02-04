library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = "demo"
  ),
  dashboardSidebar(),
  dashboardBody(
    tags$head(tags$style(HTML(
      '.myClass {
            font-size: 20px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
            padding: 0 15px;
            overflow: hidden;
            color: white;
            }
            '))),
    tags$script(HTML('
                           $(document).ready(function() {
                           $("header").find("nav").append(\'<div id="pageHeader" class="myClass"></div>\');
                           })
                           '))
  ),
  useShinyjs()
)

server <- function(input, output) {
  observeEvent(input$tabs, {
    header <- switch(input$tabs,
                     tab1 = "Tab 1",
                     tab2 = "Tab 2",
                     tab3 = "Tab 3")

    # you can use any other dynamic content you like
    shinyjs::html("pageHeader", header)
  })
}

shinyApp(ui, server)
