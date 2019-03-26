library("shiny")
library("shinydashboard")
library("highcharter")

data(citytemp)

# header <- shinydashboard::dashboardHeader(
#   tags$li(
#     class = "dropdown",
#     tags$style(".main-header {max-height: 100px;}"),
#     tags$style(".main-header .logo {max-height: 50px;}"),
#     tags$style(".sidebar-toggle {height: 20px; padding-top: 1px;}"),
#     tags$style(".navbar {min-height: 20px;}")
#   ),
#   title = tags$img(src = "momlinc_logo.PNG", height = "auto", width = 150, aling = "left")
# )
# 
# 
# sidebar <- shinydashboard::dashboardSidebar(
#   tags$style(".left-side, .main-sidebar {padding-top: 60px}"), # Add extra padding for logo
#   
#   menu_choices("about", "net_vis", "run_mcda", "results") # Add navbar elements
# )


body <- shinydashboard::dashboardBody(
  fluidPage(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://bootswatch.com/paper/bootstrap.css"),
    fluidRow(
      column(width = 3, class = "panel",
             selectInput("type", label = "Type", choices = c("line", "column", "bar", "spline")),
             selectInput("stacked", label = "Stacked", choices = c(FALSE, "normal", "percent")),
             selectInput("credits", label = "Credits enabled", choices = c(FALSE, TRUE)),
             selectInput("exporting", label = "Exporting enabled", choices = c(FALSE, TRUE)),
             selectInput("theme", label = "Theme", choices = c(FALSE, "darkunica", "gridlight", "sandsignika")),
             selectInput("ena", label = "3d enabled", choices = c(FALSE, TRUE)),
             sliderInput("alpha", "Alpha Angle", min = 0, max = 45, value = 15),
             sliderInput("beta", "Beta Angle", min = 0, max = 45, value = 15)
      ),
      column(width = 9,
             highchartOutput("hcontainer",height = "700px")
      )
    )
  )
  
)




ui <- shinydashboard::dashboardPage(dashboardHeader(), dashboardSidebar(), body, skin = "black")


server = function(input, output) {
  
  output$hcontainer <- renderHighchart({
    
    hc <- highchart() %>%
      hc_chart(type = input$type) %>%
      hc_xAxis(categories = citytemp$month) %>%
      hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>%
      hc_add_series(name = "London", data = citytemp$london) %>%
      hc_add_series(name = "New York", data = abs(citytemp$new_york))
    
    hc
    
  })
  
}

shinyApp(ui = ui, server = server)