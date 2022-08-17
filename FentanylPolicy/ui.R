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
                 href = ),
        menuItem("Original Spreadsheet", icon = icon("google-drive"), href=),
        menuItem("References", tabName = "references", icon = icon("book"))
    )
)

body <- dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #08306b;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #08306b;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #deebf7;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #08306b;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              color: #eff3ff;
                              background-color: #084594;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #c6dbef;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #fcfbfd;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #084594;
         }
                              
           .box.box-solid.box-primary>.box-header {
  #color:#000000;
  background:#08306b
                    }

.box.box-solid.box-primary{
border-bottom-color:#807dba;
border-left-color:#c6dbef;
border-right-color:#c6dbef;
border-top-color:#c6dbef;


}                   
                              
                              '))),
    
    
    #     tags$style(HTML("
    # 
    # 
    
    
    #                                   ")),
    
    
    tabItems(
        tabItem(
            # MAIN DASHBOARD ---------------------------------------------------
            tabName = "dashboard",
            ## INPUTS --------
            column(width = 3,
                   ## Population
                   box(title = "Inputs", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       selectInput("costarrest",
                                   ("State"),
                                   choices= c(paste(as.character(state.name)),"US Average"), selected="Colorado") ,
                       
                       selectInput("atRisk", ("How much fentanyl is a felony?"), 
                                   choices = list("Greater than 4g" = 1, "Greater than 1g" = 2,
                                                  "Any amount" = 3), selected = 1),
                       
                       selectInput("timeframe", ("Length of simulation?"),
                                   choices = list("One year" = 1, "Five years" = 5,
                                                  "Ten years" = 10), selected = 10),
                       
                       sliderInput("propFentanyl", 
                                   ("Proportion of the illicit drug supply that contains fentanyl"), 
                                   min=0, max=1, value=0.8),
                       
                       sliderInput("PropIncid", 
                                   ("Proportion of the overall population who uses illicit drugs that knowingly possess fentanyl"), 
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
                   fluidRow(box(textOutput("StateText"), width=16,background="navy")),
                   fluidRow(
                       valueBoxOutput("Deaths_incid", width = 4),
                       valueBoxOutput("Deaths_reg", width = 4)),
                   
                   fluidRow(valueBoxOutput("Arrests_incid", width = 4),
                            valueBoxOutput("Arrests_reg", width = 4),
                   ),
                   fluidRow(
                       valueBoxOutput("Costs_arrest", width = 6),
                       valueBoxOutput("Costs_death", width = 6)),
                   
                   fluidRow(valueBoxOutput("Costs_total", width = 6)),
                   
                   fluidRow(plotlyOutput("I_plot"), width = 400)
            )
        ),
        ## References ----------------------------------------------------------
        tabItem(
            tabName = "references"
        )
    )
)



ui <- dashboardPage(header, sidebar, body)
