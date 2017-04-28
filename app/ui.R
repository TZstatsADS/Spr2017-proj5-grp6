library(shinythemes)
library(shiny)
library(DT)
library(plotly)
library(shinydashboard)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(tidyr)
library(readr)

source("../lib/plot_radar.R")

score <- read.csv("../data/scorer2_2015-17.csv")
goal <- select(score, Player, Team, Goals, Year)
names(goal) <- c("player", "team", "goals", "year")
mins <- select(score, Player, Team, Play, Mins, Avg_mins, Year)
names(mins) <- c("player", "team", "play", "mins", "avg_mins", "year")


### data
load("../output/FIFAFull.RData")
load("../output/FIFAPrem.RData")
load("../output/Player_stats.RData")
load("../output/Fulldata15.RData")
load("../output/Fulldata16.RData")
load("../output/Fulldata.RData")
load("../output/Prem_player.RData")
load("../output/matches.RData")
#matches <- read.csv("../data/matches.csv")

## UI Function

ui<- navbarPage(
  
  ##link to css.file
  theme = "bootstrap.css",
  
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
            
                  titlePanel("Style Explorer - Clustering Analysis"),


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
                                          
                                          selectInput("season",
                                                      label = "Select the Season", 
                                                      choices = c("2015","2016"), selected="2016")),
                                      
                                          selectInput("Position",
                                                      label="Start Searching by Position", 
                                                      choices=unique(Fulldata$Pos1),
                                                      selected = "Striker"),  #choose player position
                            
                                          
                            
                                          selectInput("tag",
                                                      label=" Select Player by Play Style", 
                                                      choices=c(1,2,3)),#orig_1617$CUISINE.DESCRIPTION
                                      
                                          DT::dataTableOutput("table1")
                                          #verbatimTextOutput("selection")
                            
                                      )
                        
                                  )
                              )
                      

                      
             ),
             ### end Cluster
             
             
             ### comparison
             tabPanel("Comparison",
                      
                      titlePanel("  Comparison for Selected Players"),
                      
                                     
                                     tabsetPanel(
                                       
                                       ### 6-dim
                                       tabPanel("Scorer, Passing, Defensive",
                                                br(),
                                                br(),

                                                fluidRow(
                                                    column(3,  selectInput(
                                                          "stats1",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Goals.P")  #choose player position
                                                    ),
                                                    column(3,  selectInput(
                                                          "stats2",label="x axis stats", choices=colnames(Prem_player)[c(3,4:10)],selected = "Shots")  #choose player position
                                                    )),
                                                    
                                                fluidRow( 
                                                    column(9,plotlyOutput("plotlydef")
                                                    )),
                                                
                                                fluidRow(    
                                                    column(3,  selectInput(
                                                          "stats3",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Tackles")  #choose player position
                                                    ),
                                                    column(3,  selectInput(
                                                          "stats4",label="x axis stats", choices=colnames(Prem_player)[c(15:23)],selected = "Fouls")  #choose player position
                                                    )),
                                                
                                                fluidRow(
                                                    column(9,plotlyOutput("plotlypass")
                                                    )),
                                                
                                                fluidRow(
                                                    column(3,  selectInput(
                                                          "stats5",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Pass.Success")  #choose player position
                                                    ),
                                                    column(3,  selectInput(
                                                          "stats6",label="x axis stats", choices=colnames(Prem_player)[c(24:32)],selected = "Passes")  #choose player position
                                                    )),
                                                fluidRow(    
                                                    column(11,offset=0.6,plotlyOutput("plotlyscore")
                                                    ))
                                                
                                                
                                                #plotlyOutput("plotlydef"),
                                                #plotlyOutput("plotlypass"),
                                                #plotlyOutput("plotlyscore")                          
                                     
                                       ),
                                       
                                       ### goal/team
                                       tabPanel("Player Goals by Team",
                                                br(),
                                                br(),

                                                plotlyOutput("playergoal")
                                                
                                       ),
                                       
                                       ###avg.
                                       tabPanel("Minutes per Game",
                                                br(),
                                                br(),

                                                plotlyOutput("playertime")
                                       ),
                                                
                                       ### radar
                                       tabPanel("Comprehensive Abilities",
                                                
                                      
                                                sidebarLayout(
                                                  sidebarPanel(
                                                
                                                    
                                                    #check box of the polar chart
                                                    checkboxGroupInput('name', label = h3("Player Name"), 
                                                                       choices = Prem$Name,
                                                                       selected =""),
                                                    
                                                    
                                                    width = 3
                                                    
                                                  ),
                                                  
                                                  mainPanel(

                                                plotOutput("spider", width = "100%", height = "400px")
                                                
                                       ),
                                       position = "right"
                                       
                                   
                                   
                                       )
                                     
                                 
                                 
                                    )
                                   
                                   
                                   
                                )
                      
                      
             
             
                       
  )  ### comparison
  ),
  
  ## Team tab
  tabPanel("Team",
           titlePanel("  h2h Stats between Teams"),
           
           sidebarLayout(
             sidebarPanel(
               selectInput("Home",
                           label=" Select Home Team", 
                           choices=matches$Team,
                           selected = "Arsenal"
                          ),
             
               selectInput("Oppo",
                         label=" Select Opponent Team", 
                         choices=matches$Opponent,
                         selected = "Swansea City"
                         )
               ),
           
           
           mainPanel(
             plotOutput("Spider", width = "700px")
           ),
           position = "right"
               
             )
           )

  
  ## end team tab
)


