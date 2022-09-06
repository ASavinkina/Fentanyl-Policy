library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(plotly)


# UI for shiny app looking at felonization of fentanyl posession 

input_element_color <- "primary" 
highlight_color <- "navy" 
regular_color <- "navy"


header <- dashboardHeader(
    tags$li(
        class = "dropdown"),
    title = "Effects of policy felonizing fentanyl possession", titleWidth = 500
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Source Code", icon = icon("file-code-o"), 
                 href = "https://github.com/ASavinkina/Fentanyl-Policy"),
        menuItem("References", tabName = "references", icon = icon("book"))
    )
)

body <- dashboardBody(
#     tags$head(tags$style(HTML('
#         /* logo */
#         .skin-blue .main-header .logo {
#                               background-color: #08306b;
#                               }
# 
#         /* logo when hovered */
#         .skin-blue .main-header .logo:hover {
#                               background-color: #08306b;
#                               }
# 
#         /* navbar (rest of the header) */
#         .skin-blue .main-header .navbar {
#                               background-color: #deebf7;
#                               }        
# 
#         /* main sidebar */
#         .skin-blue .main-sidebar {
#                               background-color: #08306b;
#                               }
# 
#         /* active selected tab in the sidebarmenu */
#         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
#                               color: #eff3ff;
#                               background-color: #084594;
#                               }
# 
#         /* other links in the sidebarmenu */
#         .skin-blue .main-sidebar .sidebar .sidebar-menu a{
#                               background-color: #c6dbef;
#                               color: #000000;
#                               }
# 
#         /* other links in the sidebarmenu when hovered */
#          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
#                               background-color: #fcfbfd;
#                               }
#         /* toggle button when hovered  */                    
#          .skin-blue .main-header .navbar .sidebar-toggle:hover{
#                               background-color: #084594;
#          }
#                               
#            .box.box-solid.box-primary>.box-header {
#   #color:#000000;
#   background:#08306b
#                     }
# 
# .box.box-solid.box-primary{
# border-bottom-color:#807dba;
# border-left-color:#c6dbef;
# border-right-color:#c6dbef;
# border-top-color:#c6dbef;
# 
# 
# }                   
#                               
#                               '))),
#     

    
    
    tabItems(
        tabItem(
            # MAIN DASHBOARD ---------------------------------------------------
            tabName = "dashboard",
            ## INPUTS --------
            column(width = 4,
                   ## Population
                   box(title = "Inputs", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       selectInput("costarrest",
                                   ("State"),
                                   choices= c(paste(as.character(state.name)),"US Average"), selected="Colorado") ,
                       
                       selectInput("atRisk", ("How much fentanyl is a felony?"), 
                                   choices = list("Greater than 4g" = "1", "Greater than 1g" = "2",
                                                  "Any amount" = "3"), selected = "1"),
                       
                       selectInput("timeframe", ("Length of simulation?"),
                                   choices = list("One year" = 1, "Five years" = 5,
                                                  "Ten years" = 10), selected = 1),
                       
                       sliderInput("propFentanyl", 
                                   ("Proportion of the illicit drug supply that contains fentanyl"), 
                                   min=0, max=1, value=0.5),
                       
                       
                       selectInput("Arrests", 
                                   ("Policing strategy"), 
                                   choices= list("Standard Policing"= 0.13, "Aggressive Policing"= 0.3,
                                                 "Treatment-informed Policing"=0.05), selected= "Standard Policing"),
                       
                       selectInput("costdeath", 
                                   ("Costs associated with a death"), 
                                   choices= list("Healthcare costs only", "Lost productivity costs only",
                                                 "Value of a statistical life"), selected= "Value of a statistical life")
                   )
            ),
            
            ## OUTPUT: plot and metrics --------
            
        column(width = 8,
               fluidRow(box(textOutput("StateText"), 
                            width=10,
                            background="navy")),


                   fluidRow(valueBoxOutput("Overdoses", width=5),
                   valueBoxOutput("Mortality", width=5)),


                   fluidRow(valueBoxOutput("Arrests1", width=5),
                   valueBoxOutput("IncarcerationKnown", width=5)),


                   fluidRow(
                       valueBoxOutput("Costs2", width=5),
                            valueBoxOutput("Loss", width=5)),

                   fluidRow(valueBoxOutput("TotalCosts", width=10)),


               fluidRow(plotlyOutput("I_plot"))
        )),
    
        # ,
        ## References ----------------------------------------------------------
          tabItem(
              tabName = "references",
              h2("Sources"),
              p(strong("Population in model:"),"The model was populated according to estimates
                of people with OUD in each state and the US as a whole, as estimated by Krawczyk et al. 2022 
                with data from the National Survey on Drug Use and Health. See:", 
                a("Krawczyk et al. 2022",
                  href = "https://www.sciencedirect.com/science/article/pii/S0955395922002031?via%3Dihub")),
              p(strong("Percent of people with OUD who unknowingly possess fentanyl:"),
                "Estimated at 60% based on literature sources. See:", 
                a("Macmadu et al. 2017",
                  href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5291510/"),
                a("Carroll et al. 2017",
                href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5560423/")),
              p(strong("Amount of drug in possession:"),
                    "The proportion of the population that possess >4 grams of drugs, between 1 and 4 grams,
                    and below 1 gram. Based on drug arrest data from 2004, 2008, and 2012. See:", 
                a("Kennedy et al. 2018.",
                  href = "https://lawreview.law.ucdavis.edu/issues/52/2/Articles/52-2_Kennedy.pdf")),
              p(strong("Annual overdose probability:"),"Estimated at around 0.8% annually, based on data from 
                CDC on number of overdoses annually and NSDUH for number of people with OUD in the US. See ", 
                a("CDC SARS-CoV-2 Diagnostic, Screening, and Surveillance Testing.",
                  href = "https://www.cdc.gov/nchs/pressroom/nchs_press_releases/2021/20211117.htm")),
              p(strong("Overdose multiplier following incarceration:"),"For first month post-release: 40x.
                For the next 11 months: 10x. Then back to baseline. Based on literature estimates. See:",
                a("Ranapurwala et al. 2018",
                  href ="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6085027/")),
            p(strong("Annual arrest probability:"),"Estimated at 13% based on arrest numbers for possession
              and population with drug use in the US. See:",
                a("PEW Charitable Trusts",
                    href = " https://www.pewtrusts.org/-/media/assets/2022/02/drug-arrests-stayed-high-even-as-imprisonment-fell-from-2009-to-2019.pdf")),
            p(strong("Multiplier on subsequent arrests:"),"2.43x, based on literature estimates. See:",
                a("Belenko et al. 2013",
                 href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3859122/")),
             p(strong("Cost of incarceration, by state:"),"Estimated from Vera research on prison spending. See:",
                a("Vera, Prison Spending in 2015",
                    href = "https://www.vera.org/publications/price-of-prisons-2015-state-spending-trends/price-of-prisons-2015-state-spending-trends/price-of-prisons-2015-state-spending-trends-prison-spending")),
             p(strong("Cost of an overdose death:"),"Estimated from literature sources. Healthcare only: $5,500,
               lost productivity: $1.4 million, value of a statistical life: $10 million. See:",
                a("Florence et al. 2021",
                    href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8091480/")))
          
     )
)



ui <- dashboardPage(header, sidebar, body)
