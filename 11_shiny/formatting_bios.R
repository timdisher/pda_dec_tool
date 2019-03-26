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
    )

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
           
  p("Welcome to the interactive webapp for CNPRM poster P30:",
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
  
  tags$iframe(style="height:400px; width:100%", src="poster.pdf"),

  h3("About the Authors"),
  
  #Tim's Bio---- -


  tags$img(src = "headshots/tim_pic.PNG", height = "25%", width = "25%", align = "left", style = "max-height: 214px; max-width: 210px",
           hspace = 20, vspace = 5),
  
  h3("Tim Disher, BScN, RN, PhD(c)"),
  p("A doctoral student in the PhD in Nursing program at Dalhousie University, 
    and a 2017 CIHR Vanier Scholar. Tim\'s research examines medical decision making,
    drawing on the fields of evidence synthesis and decision analysis."),
  
  br(clear = "left"),
  tags$hr(),
 
  #Souvik's Bio---- -
  
  tags$img(src = "headshots/souvik_pic.jpg", height = "25%", width = "25%", align = "left", style = "max-height: 260px; max-width: 214px",
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
  
  tags$img(src = "headshots/louis_pic.jpg", height = "25%", width = "25%", align = "left", style = "max-height: 214px; max-width: 210px",
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
  
  tags$img(src = "headshots/marsha_pic.PNG", height = "25%", width = "25%", align = "left", style = "max-height: 214px; max-width: 210px",
           hspace = 20, vspace = 5),
  
  h3("Marsha Campbell-Yeo, RN, MN.NNP-BC, PhD"),
  p("Dr. Campbell-Yeo, a neonatal nurse practitioner and clinician scientist, is an Associate Professor 
     at the School of Nursing, Faculty of Health, Dalhousie University and holds cross appointments in the 
     Department of Pediatrics, and Psychology and Neuroscience. Her Canada Foundation of Innovation funded 
     research and interdisciplinary training lab, MOM-LINC (Mechanisms, Outcome and Mobilization of 
     Maternally-Led Interventions to Improve Newborn Care), is located at the IWK Health Centre. 
     She primarily holds grants examining maternally-led interventions to improve outcomes of medically 
     at-risk newborns specifically related to pain, stress and neurodevelopment as well as novel knowledge 
     synthesis and dissemination, and e-heath interventions most notably related to parental engagement."),
  
    p("She has been recognized for her contributions to the field via numerous training, leadership, and research 
    awards. Most notably, as the recipient of the Inaugural 2018 Dalhousie University President\'s Award for 
    research excellence, was named one of 150 Nurses championing innovation in health for Canada by the Canadian 
    Nurses Association to mark the 150th anniversary of Confederation (2017), invited as a member of the Royal 
    Society of Canada\'s College of New Scholars, Artists and Scientists (2017), a Canadian Institute of Health 
    Research New Investigator Award (2016- 2021), the Canadian Pain Society 2015 Early Career Award, and a 
    Career Development Award from the Canadian Child Health Clinician Scientist Program (2013-2017) . She is 
    the current Treasurer and a Council Member of the Pain in Childhood Special Interest Group of the 
    International Society for the Study of Pain and past Secretary of the Canadian Pain Society Board of Directors.
    ")
  )
  )
)
  
  
}




#====================================================================================== =
#
# Menubar navigation items
#
#====================================================================================== =

menu_choices <- function(about, net_vis, run_mcda,  results = NULL){
  sidebarMenu(
    id = "sidebar",
    menuItem("About", tabName = about, icon = icon("question-circle")),
    menuItem("Network Visualization", tabName = net_vis, icon = icon("connectdevelop")),
    menuItem("Run Analysis", tabName = run_mcda, icon = icon("plane-departure"))
    #menuItem("Results", tabName = results, icon = icon("chart-bar"))
  )
}
