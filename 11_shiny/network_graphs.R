# Tab structure for network graph tab
net_content <- function(){
  tabItem(tabName = "net_vis",
          h2("Network Graphs"),
          p("For each network graph, nodes represent treatments and links represent
            comparisons. The size of the node is proportional the total number of patients
            who received the treatment. The width of links are proportional to the number
            of studies comparing two treatments. "),
          tabBox(width = 12, id = "net_box",
                             net_pda_closure(),
                             net_sx(),
                             net_mort(),
                             net_nec()
                             )
          )
}

net_pda_closure <- function(){
  tabPanel("PDA Closure",
           fluidPage(h2("Patent ductus arteriosus closure"), h4("60 trials; 4256 infants"), 
                     tags$img(src = "network_graphs/pda_netgraph.PNG", height = "100%", width = "100%", style = "max-height: 1014px; max-width: 746px")))
           
}

net_sx <- function(){
  tabPanel("Surgical ligation",
           fluidPage(h2("Need for surgical patent ductus arteriosus ligation"), h4("37 trials; 2729 infants"), 
                     tags$img(src = "network_graphs/sx_netgraph.PNG", height = "100%", width = "100%", style = "max-height: 1014px; max-width: 746px")))
  
}

net_mort <- function(){
  tabPanel("Mortality",
           fluidPage(h2("Neonatal Mortality"), h4("46 trials; 3329 infants"), 
                     tags$img(src = "network_graphs/mort_netgraph.PNG", height = "100%", width = "100%", style = "max-height: 1014px; max-width: 746px")))
  
}
net_nec <- function(){
  tabPanel("NEC",
           fluidPage(h2("Necrotizing enterocolitis"), h4("45 trials; 3371 infants"), 
                     tags$img(src = "network_graphs/nec_netgraph.PNG", height = "100%", width = "100%", style = "max-height: 1014px; max-width: 746px")))
  
}



