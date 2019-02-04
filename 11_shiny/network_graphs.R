# Tab structure for network graph tab
net_content <- function(){
  tabItem(tabName = "net_vis",
          h2("Network Graphs"),
          p("For each network graph, nodes represent treatments and links represent
            comparisons. The size of the node is proportional the total number of patients
            who received the treatment. The width of links are proportional to the number
            of studies comparing two treatments. "),
          shiny::tabsetPanel(type = "tabs",
                             net_pda_closure(),
                             net_rpt_trt(),
                             net_sx(),
                             net_mort(),
                             net_nec(),
                             net_bpd(),
                             net_ivh(),
                             net_oligo()
                             )
          )
}

net_pda_closure <- function(){
  tabPanel("PDA Closure",
          tags$div(id= "pda_net",
                   style="min-width: 320px; 
                          max-width: 500px; 
                          height: 600px; 
                          margin: 0 auto
                          margin-left: 0"),

         tags$script(type = 'text/javascript', src = "network_graphs/network_chart_pda.js")
           )
           
}

net_rpt_trt <- function(){
  tabPanel("Need for repeat pharmacotherapy",
           tags$div(id= "rpt_rx_net",
                    style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_rpt_rx.js")
  )
           
}

net_sx <- function(){
  tabPanel("Need for surgical ligation",
           tags$div(id= "sx_net",
                    style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_sx.js")
  )
}

net_mort <- function(){
  tabPanel("Neonatal mortality",
           tags$div(id= "mort_net",
                    style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_mort.js")
           )
}

net_nec <- function(){
  tabPanel("Necrotizing enterocolitis",
           tags$div(id= "nec_net",
                    style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_nec.js")
           )
}

net_bpd <- function(){
  tabPanel("Bronchopulmonary Dysplasia",
           tags$div(id= "bpd_net",
                    style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_bpd.js")
  )
}

net_ivh <- function(){
  tabPanel("Intraventricular Hemorrhage",
           tags$div(id= "ivh_net",
                   style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_ivh.js")
  )
}

net_oligo <- function() {
  tabPanel("Oliguria",
           tags$div(id= "oligo_net",
                    style="min-width: 50px; 
                    max-width: 500px; 
                    height: 400px; 
                    margin: 0 auto
                    margin-left: 0"),
           
           tags$script(type = 'text/javascript', src = "network_graphs/network_chart_oliguria.js")
  )
}
