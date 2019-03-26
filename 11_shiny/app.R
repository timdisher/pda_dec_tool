library(shiny)
library(shinydashboard)
library(smaa)
library(hitandrun)
library(highcharter)
library(tidyverse)
library(memisc)
source("formatting_bios.R")
source("network_graphs.R")
source("mcda_functions.R")
source("mcda_tab.R")
load("ordinal_weightsv2.rda")
load("p_bar_datv2.rda")
load("reg_out_resv2.rda")

data(citytemp)

header <- shinydashboard::dashboardHeader(
  tags$li(
          class = "dropdown",
          tags$style(".main-header {max-height: 200px;}"),
          tags$style(".main-header .logo {max-height: 50px;}"),
          tags$style(".sidebar-toggle {height: 50px; padding-top: 1px;}")
          ),
  title = tags$img(src = "momlinc_logo.PNG", height = "auto", width = 150, aling = "left")
)


sidebar <- shinydashboard::dashboardSidebar(
 tags$style(".sidebar-toggle {max-height: 200px}"), # Add extra padding for logo

  menu_choices("about", "net_vis", "run_mcda") # Add navbar elements
)


body <- shinydashboard::dashboardBody(
  format_edits(), # Set dashboard format, header colours, logo, etc...

   tabItems( # Create UI elements for nav panel
    # First tab content
    about_content(),

    # # Network graphs
    # 
    net_content(), 

    # MCDA tab
    mcda_content()

    

  )

)

ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output) {
  # Custom ranking inputs that ensure nothing is double selected
  output$second_rank <- renderUI({selectInput(inputId = "second", label = "Second most important outcome", choices = out_choices[!out_choices %in% input$first])})
  output$third_rank <- renderUI({selectInput(inputId = "third", label = "Third most important outcome", choices = out_choices[!out_choices %in% c(input$first, input$second)])})
  output$last_rank <- renderUI({selectInput(inputId = "last", label = "Least important outcome", choices = out_choices[!out_choices %in% c(input$first, input$second, input$third)])})
  
  #Data dictionary
  nice_names <- data.frame(data = c("INDOIV", "IBUIV", "IBUPO", "PARAPO", "IBUIVHIGHDOSE",
                                    "IBUPOHIGHDOSE", "IBUIVCONT", "INDOIVCONT", "INDOTHERS",
                                    "PLAC_NORX"),
                           nice = c("Indomethacin (IV)", "Ibuprofen (IV)", "Ibuprofen (PO)",
                                    "Paracetamol (PO)", "Ibuprofen (IV, high dose)", "Ibuprofen (PO, high dose)",
                                    "Ibuprofen (Cont. IV)", "Indomethacin (Cont. IV)", "Indomethacin (Others)",
                                    "Placebo/No Rx"))
  
   d_s <- d_s
   w <- w

  # Ranks
   
  ranks <- reactive({
   
    if(input$ranks == "clin1"){

    out =   tibble(outcome = c("mort", "nec", "sx", "closure"),
           rank = 1:4)
    }
   
    if(input$ranks == "clin2"){
      
     out =  tibble(outcome = c("mort", "closure", "nec", "sx"),
                      rank = 1:4)
    }
    
    if(input$ranks == "custom"){
    
     out =    tibble(outcome = c(input$first, input$second, input$third, input$last),
                      rank = 1:4)
    }
   out 
  }) 
  

  # Reset button
  observeEvent(input$reset_p, {
    shinyjs::reset("p_base")
  })
  
   
   
  #Absolute probabilities
  p_s <- reactive({

   out <- rel_to_ab(p_clos = input$p_clos/100,
                   p_sx = input$p_sx/100,
                   p_mort = input$p_mort/100,
                   p_nec = input$p_nec/100,
                   d_s = d_s,
                   p_bar = FALSE)
    out
    })

  # Run mcda (reactive but should be isolated with action button)

  results <- reactive({   
    run_smaa(ps_array = p_s(),
                      ranks = ranks(),
                      w = w)})
  



#   # Create plots
output$rankogram <- renderHighchart({
  
  input$run_mcda
  
  results <- isolate(results())
  class(results$smaa$ra) <- "matrix"

  out <- results$smaa$ra %>% as.data.frame() %>% rownames_to_column() %>% purrr::set_names(c("var",paste0("r",1:10))) %>% arrange(-r1) %>%
    mutate_at(vars(r1:r10), ~ . * 100)

  out$var <- nice_names[match(out$var, nice_names$data), 2]

 hc <- highchart() %>%
    hc_xAxis(categories = out$var, title = list(text = "Treatment")) %>%
    hc_yAxis(max = 100, labels = list(format = '{value} %')) %>%
    hc_add_series(name = "Rank 10", data = out$r10, type = "column")%>%
    hc_add_series(name = "Rank 9", data = out$r9, type = "column")%>%
    hc_add_series(name = "Rank 8", data = out$r8, type = "column")%>%
    hc_add_series(name = "Rank 7", data = out$r7, type = "column")%>%
    hc_add_series(name = "Rank 6", data = out$r6, type = "column")%>%
    hc_add_series(name = "Rank 5", data = out$r5, type = "column")%>%
    hc_add_series(name = "Rank 4", data = out$r4, type = "column")%>%
    hc_add_series(name = "Rank 3", data = out$r3, type = "column")%>%
    hc_add_series(name = "Rank 2", data = out$r2, type = "column")%>%
    hc_add_series(name = "Rank 1", data = out$r1, type = "column")%>%
    hc_tooltip(valueDecimals = 0,
               valueSuffix = "%") %>%
    hc_plotOptions(series=list(stacking='normal')) %>%
    hc_exporting(enabled = TRUE)


 hc
})

info_boxes <- reactive({

  input$run_mcda
  
  results<- isolate(results())
  
  class(results$smaa$ra) <- "matrix"
  
  first <- results$smaa$ra %>% as.data.frame() %>% rownames_to_column("trt") %>% dplyr::select(trt, V1) %>% arrange(-V1) %>% slice(1) %>% mutate(V1 = V1 * 100)
  second <- results$smaa$ra %>% as.data.frame() %>% rownames_to_column("trt") %>% dplyr::select(trt, V1) %>% arrange(-V1) %>% slice(2) %>% mutate(V1 = V1 * 100)
  cf_1 <- round(results$confidence$cf[which(names(results$confidence$cf) == first$trt)] * 100, 0)
  cf_2 <- round(results$confidence$cf[which(names(results$confidence$cf) == second$trt)] * 100, 0)
  
  out <- list(first = first, second = second, cf_1 = cf_1, cf_2 = cf_2)
})


output$box_first_name  <- renderText({

  as.character(nice_names[match(info_boxes()$first$trt, nice_names$data), 2])
  
})

output$box_first_val <- renderText({

  paste0(round(info_boxes()$first$V1, 0), "%", " (Confidence = ",round(info_boxes()$cf_1), " %)")
  
  })


output$box_second_name  <- renderText({
  
  as.character(nice_names[match(info_boxes()$second$trt, nice_names$data), 2])
  
})

output$box_second_val <- renderText({

  
  paste0(round(info_boxes()$second$V1, 0), "%", " (Confidence = ",round(info_boxes()$cf_2), " %)")
  
})



#Bar plot

output$barplot <- renderHighchart({
#browser()
  p_bar <-  rel_to_ab(p_clos = input$p_clos/100,
                          p_sx = input$p_sx/100,
                          p_mort = input$p_mort/100,
                          p_nec = input$p_nec/100,
                          d_s = d_s,
                          p_bar = TRUE)

  p_bar$closure$trt <- nice_names[match(p_bar$closure$trt, nice_names$data), 2]

  highchart() %>%
    hc_xAxis(categories = p_bar$closure$trt, title = list(text = "Treatment")) %>%
    hc_yAxis(max = 100, title = list(text = "Probability of event"), labels = list(format = '{value} %')) %>%

    hc_add_series(data = p_bar$closure$med, type = "column", name = "PDA Closure") %>%
    hc_add_series(data = list_parse(p_bar$closure[c(2,4)]),
                  type = "errorbar") %>%

    hc_add_series(data = p_bar$nec$med, type = "column", name = "NEC") %>%
    hc_add_series(data = list_parse(p_bar$nec[c(2,4)]),
                  type = "errorbar") %>%

    hc_add_series(data = p_bar$mort$med, type = "column", name = "Mortality") %>%
    hc_add_series(data = list_parse(p_bar$mort[c(2,4)]),
                  type = "errorbar") %>%

    hc_add_series(data = p_bar$sx$med, type = "column", name = "Surgical ligation") %>%
    hc_add_series(data = list_parse(p_bar$sx[c(2,4)]),
                  type = "errorbar") %>%
    hc_tooltip(valueDecimals = 0,
               valueSuffix = "%") %>%
    hc_exporting(enabled = TRUE)

})


 
  
}


shiny::shinyApp(ui, server)
