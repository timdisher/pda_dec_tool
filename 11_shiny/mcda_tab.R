# Make ranks reactive
mcda_content <- function(){
  tabItem(tabName = "run_mcda",
          mcda_tab()
  )
}

# Outcome 1
mcda_tab <- function(output){
  
  
  shiny::fluidPage(
    shiny::fluidRow(
      column(width = 8,
             tabBox(title = "Results", width = 12, id = "tab_box1",
                    tabPanel("Absolute outcomes",  highchartOutput("barplot", height = "800px")),
                    
                     tabPanel("Rankogram", highchartOutput("rankogram", height = "800px")
                              )
             ), mcda_value_boxes()
             
      ), column(width = 4,
               box(title = "Visualization controls", width = NULL,
                   mcda_chart_controls()))
    )
      
    )
  
}


mcda_value_boxes <- function(){
  fluidRow(
  column(width = 6, infoBox(title = textOutput("box_first_name"), textOutput("box_first_val"), width = NULL, subtitle = "First rank acceptability and confidence factor of top ranked treatment", icon = icon("trophy")))
           ,
  column(width = 6, infoBox(title = textOutput("box_second_name"), textOutput("box_second_val"), width = NULL, subtitle = "First rank acceptability of second ranked treatment", icon = icon("medal")))
           
    
  )
}

#Choices for outcome ranking
out_choices <- c("PDA Closure" = "closure", "Surgical ligation" = "sx", "NEC" = "nec", "Mortality" = "mort")

mcda_chart_controls <- function(input){

  
fluidPage(
  fluidRow(
  h2("Outcome ranking"),
  p("Rank outcomes from most to least important for decision making. Your ranking can consider
    importance of the outcome alone, or a combination of importance and your impression of the trustworthiness
    of the NMA estimate"),
  
  # Pre-set choices
  selectInput("ranks", label = "Poster pre-sets or custom",
               choices = list("Clinician 1 (Mortality, NEC, Surgical closure, PDA closure)" = "clin1",
                           "Clinician 2 (Mortality, PDA closure, NEC, Surgical closure)" = "clin2",
                           "Custom (choose your own)" = "custom")
              ),
  
  # Pull-down menues

conditionalPanel(condition = "input.ranks == 'custom'",
                 
  selectInput(inputId = "first", label = "Most important outcome", choices = out_choices, selected = NA),
  
  uiOutput("second_rank"), #inputID = "second"
  uiOutput("third_rank"), #inputID = "third"
  uiOutput("last_rank") #intputID = "last"
  
    )
  ), tags$hr(), fluidRow(
    shinyjs::useShinyjs(),
    id = "p_base",
    h2("Baseline event rates"),
    p("Provide baseline event probabilities for each outcome of interest. These should be in reference
      to treatment with IV indomethacin"),
    
    #Slider inputs
    sliderInput("p_clos", "PDA Closure", min = 0, max = 100, post = " %", value = 25),
    sliderInput("p_sx", "Surgical ligation", min = 0, max = 100, post = " %", value = 12),
    sliderInput("p_mort", "Mortality", min = 0, max = 100, post = " %", value = 12),
    sliderInput("p_nec", "NEC", min = 0, max = 100, post = " %", value = 9),
    actionButton("reset_p", "Reset probabilities"),
    tags$hr(),
    actionButton("run_mcda", "Run MCDA")
  )
)
}