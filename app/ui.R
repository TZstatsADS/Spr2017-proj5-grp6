## Packages
library(shinythemes)
library(shiny)
library(DT)
library(plotly)
library(shinydashboard)
library(leaflet)

load("../output/FIFAFull.RData")
load("../output/FIFAPrem.RData")
load("../output/Player_stats.RData")

## UI Function

ui<- navbarPage(
  
  ##link to css.file
  theme = "bootstrap2.css",
  
  ##Project Title
  "Soccer Manager",

  ##Home
  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("Soccer Manager"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
           ),


  ## Player tab
  navbarMenu("Player",
             
             
             
             ##Cluster tabset
             tabPanel("Position & Play Style",
            
                  titlePanel("Find Style - Clustering Analysis"),


                      body <- dashboardBody(

                              fluidRow(
                                  column(width = 9,
                                      box(width = NULL, solidHeader = TRUE,
                                          plotlyOutput("barPlot",height = 400)
                                      ),
                                      box(width = NULL,
                                          DT::dataTableOutput("table")
                                      )
                                  ),

                                  column(width = 3,
                                      box(width = NULL, status = "warning",
                            
                                          selectInput("Position",
                                                      label="Start searching by position", 
                                                      choices=unique(Prem$Pos1)),  #choose player position
                            
                                          DT::dataTableOutput("table1"),
                            
                                          selectInput("tag",
                                                      label=" Select Player by play style", 
                                                      choices=c(1,2,3)),#orig_1617$CUISINE.DESCRIPTION
                                          verbatimTextOutput("selection")
                            
                                      )
                        
                                  )
                              )
                      )

                      
             ),
             ### end Cluster
             
             
             ### comparison
             tabPanel("Comparison",
                      
                      titlePanel("Comparison for selected players"),
                      
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     
                                     tabsetPanel(
                                       
                                       ### 6-dim
                                       tabPanel("scorer, passing, defensive",
                                                titlePanel("scorer, passing, defensive")
                                     
                                       ),
                                       
                                       ### goal/team
                                       tabPanel("Goal/Team",
                                                titlePanel("Goal/Team")
                                                
                                       ),
                                       
                                       ###avg.
                                       tabPanel("avg.",
                                                titlePanel("avg.")
                                       ),
                                                
                                       ### radar
                                       tabPanel("radar",
                                                titlePanel("radar")
                                       )
                                       
                                   
                                   
                                       )
                                     
                                 
                                 
                                    ),
                                   
                                   position = "right"
                                    ### comparison


                



                                )
                      
                      
             )
             
             
             ### Motion Chart
             #tabPanel("Motion Chart",
             #         mainPanel(
             #           
             #         )
             #)
             ### end Motion Chart
             ## end Player tab
             
  ),
  
  ## Team tab
  tabPanel("Team",
           titlePanel("Team Comparison")
           
           
           
           
  )
  
  ## end team tab
)

