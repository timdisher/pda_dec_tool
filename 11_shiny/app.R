library(shiny)
library(shinydashboard)
source("formatting_bios.R")
source("network_graphs.R")

header <- shinydashboard::dashboardHeader(
  tags$li(
          class = "dropdown",
          tags$style(".main-header {max-height: 100px;}"),
          tags$style(".main-header .logo {max-height: 50px;}"),
          tags$style(".sidebar-toggle {height: 20px; padding-top: 1px;}"),
          tags$style(".navbar {min-height: 20px;}")
          ),
  title = tags$img(src = "momlinc_logo.PNG", height = "auto", width = 150, aling = "left")
)


sidebar <- shinydashboard::dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 60px}"), # Add extra padding for logo

  menu_choices("about", "net_vis", "run_mcda", "results") # Add navbar elements
)


body <- shinydashboard::dashboardBody(
  format_edits(), # Set dashboard format, header colours, logo, etc...
  
   tabItems( # Create UI elements for nav panel
    # First tab content
    about_content(),
    
    # Network graphs
    
    net_content()

  )
)


ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output) {
}

shiny::shinyApp(ui, server)
