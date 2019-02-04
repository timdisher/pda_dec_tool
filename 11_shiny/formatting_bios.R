#====================================================================================== =
#
# Edits the default formatting
#
#====================================================================================== =
format_edits <- function(){
tagList(
  tags$head(
    tags$style(
      HTML(".skin-black .main-header .logo {
           background-color: #FFFAE3;}
                            
           .skin-black .main-header .logo:hover {
           background-color: #FFFAE3;}
                            
           .skin-black .main-header .navbar {
           background-color: #FFFAE3}
                            
           /* main sidebar */
           .skin-black .main-sidebar {
           background-color: #655773;
                            }
                            
           /* active selected tab in the sidebarmenu */
           .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
           background-color: #3b4060;
                            }
                            
           /* other links in the sidebarmenu */
           .skin-black .main-sidebar .sidebar .sidebar-menu a{
           color: white;
                            }
                            
           .content-wrapper, .right-side{
           background-color: white;
            float: top;}"
           )
         )
       ),

  tags$head(tags$style(HTML(
    '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 15px;
        overflow: hidden;
        color: #4D4D4D;
      }
    '
         )
        )
      ),

  tags$script(
    HTML('
        $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> MCDA-SMAA in PDA </span>\');
      })
     ')
    ),
  
  tags$script(src = "lib/highcharts.js"),
  tags$script(src = "lib/exporting.js"),
  tags$script(src = "lib/export-data.src.js"),
  tags$script(src = "lib/networkgraph.js")
)}

#====================================================================================== =
#
# Biography content
#
#====================================================================================== =

about_content <- function(){
  tabItem(
  tabName = "about",
  
  fluidPage(
  
  fluidRow(style='padding-left:25px;',
           
  p("Welcome to the interactive webapp for CNPRM poster ID 0281_0409_000102:",
    tags$b("\"Variations in Practice May be Evidence-Based: Application of Multi-Criteria 
    Decision Analysis to Treatments for Patent Ductus Arteriosus\"."), 
    "The app has three main features that are accessible from the sidebar and will allow you to:",
       tags$ol(
         tags$li("Explore the evidence networks for each of the outcomes"),
         tags$li("See how your outcome preferences can lead to different decisions than
                 those presented in poster"),
         tags$li("Explore how rates of events at your own centre may change affect
                 decisions for optimal treatment.")
       ),
    "To get started use the navigation bar on the left to explore the different modules"),
  

  h3("About the Authors"),
  
  #Tim's Bio---- -


  tags$img(src = "headshots/tim_pic.PNG", height = "25%", width = "25%", align = "left", 
           hspace = 20, vspace = 5),
  
  h3("Tim Disher, BScN, RN, PhD(c)"),
  p("A doctoral student in the PhD in Nursing program at Dalhousie University, 
    and a 2017 CIHR Vanier Scholar. Tim\'s research examines medical decision making,
    drawing on the fields of evidence synthesis and decision analysis."),
  
  br(clear = "left"),
  tags$hr(),
 
  #Souvik's Bio---- -
  
  tags$img(src = "headshots/souvik_pic.jpg", height = "25%", width = "25%", align = "left", 
           hspace = 20, vspace = 5),
  
  h3("Souvik Mitra, MD, MSc, RCPSC Affiliate"),
  p("Dr. Souvik Mitra is an Assistant Professor at Dalhousie University with interest in 
     functional echocardiography as well as clinical and epidemiological research.  
     He received his MSc in Clinical Epidemiology from McMaster University in 2018 and is 
     currently pursuing his PhD in Epidemiology & Applied Health Research from Dalhousie 
     University.  Dr. Mitra is a member of the Fetus and Newborn Committee of the Canadian 
     Pediatric Society, the Pan-American Hemodynamics Collaborative Group, the IWK Health 
     Centre Scientific Review Committee and is an Executive Board member and social media 
     editor of the International Society for Evidence-Based Neonatology (EBNEO).  
     He is also an author and peer reviewer for the Cochrane Neonatal group. Dr. Mitra\'s 
     ongoing research projects include examining the effects of caregiving interventions 
     and medications on cerebral perfusion in preterm infants, evidence synthesis on 
     optimal management strategies for symptomatic patent ductus arteriosus as well as 
     delivery room interventions to prevent morbidity and mortality in preterm infants.
    "),
  
  br(clear = "left"),
  tags$hr(),
  
  #Louis' Bio---- -
  
  tags$img(src = "headshots/louis_pic.jpg", height = "25%", width = "25%", align = "left", 
           hspace = 20, vspace = 5),
  
  h3("Louis Beaubien, PhD"),
  p("Dr. Beaubien\'s research interests revolve around the design of financing and 
    governance models, and information system strategy in a variety of organizations, 
    most recently, the co-operative and healthcare sectors. Examples include work on 
    governance in new information system deployment, performance management and governance 
    in financial services organizations, and financing models in healthcare."),
  
  br(clear = "left"),
  tags$hr(),
  
  #Marsha's Bio---- -
  
  tags$img(src = "headshots/marsha_pic.PNG", height = "25%", width = "25%", align = "left", 
           hspace = 20, vspace = 5),
  
  h3("Marsha Campbell-Yeo, RN, MN.NNP-BC, PhD"),
  p("Leading national and international interdisciplinary research teams consisting
    of clinicians, researchers and stakeholders from diverse backgrounds and disciplines,
    Dr. Campbell-Yeo's passion has led her all over the world, and her home is MOM-LINC lab.")
  )
  )
)
  
  
}




#====================================================================================== =
#
# Menubar navigation items
#
#====================================================================================== =

menu_choices <- function(about, net_vis, run_mcda, results){
  sidebarMenu(
    id = "sidebar",
    menuItem("About", tabName = about, icon = icon("question-circle")),
    menuItem("Network Visualization", tabName = net_vis, icon = icon("connectdevelop")),
    menuItem("Run Analysis", tabName = run_mcda, icon = icon("plane-departure")),
    menuItem("Results", tabName = results, icon = icon("chart-bar"))
  )
}
