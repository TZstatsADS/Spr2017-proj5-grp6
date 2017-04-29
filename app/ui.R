##### UI for Soccer Manager #####
##### Author: ADS-Proj5-Group6 #####
##### Date: April 28,2017 #####


#### load the packages
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

source("plot_radar.R")

score <- read.csv("scorer2_2015-17.csv")
goal <- select(score, Player, Team, Goals, Year)
names(goal) <- c("player", "team", "goals", "year")
mins <- select(score, Player, Team, Play, Mins, Avg_mins, Year)
names(mins) <- c("player", "team", "play", "mins", "avg_mins", "year")


### data
load("FIFAFull.RData")
load("FIFAPrem.RData")
load("Player_stats.RData")
load("Fulldata15.RData")
load("Fulldata16.RData")
load("Fulldata.RData")
load("Prem_player.RData")
load("matches.RData")
load("pos_table.RData")




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
             

             ##Position & Play Style tabset
             tabPanel("Position & Play Style",
            
                  ##Cluster
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
                                                      choices=c(1,2,3)),
                                      
                                          DT::dataTableOutput("table1")
                                          #verbatimTextOutput("selection")
                            
                                      )
                        
                                  )
                              )
                      

                      
               ),
             ### end Cluster
             
             
             ### comparison tabset
             tabPanel("Comparison",
                      
                      titlePanel("Comparison for Selected Players"),
                      
                                     
                                     tabsetPanel(
                                       
                                       ### 6-dim
                                       tabPanel("Scorer, Defensive, Passing",
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
                                                br(),
                                                br(),
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
                                                br(),
                                                br(),
                                                fluidRow(
                                                    column(3,  selectInput(
                                                          "stats5",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Pass.Success")  #choose player position
                                                    ),
                                                    column(3,  selectInput(
                                                          "stats6",label="x axis stats", choices=colnames(Prem_player)[c(24:32)],selected = "Passes")  #choose player position
                                                    )),
                                                fluidRow(    
                                                    column(9,offset=0.6,plotlyOutput("plotlyscore")
                                                    ))                      
                                     
                                       ),
                                       ### end 6-dim


                                       ### goal/team
                                       tabPanel("Player Goals by Team",
                                                br(),
                                                br(),

                                                plotlyOutput("playergoal", height = "500px")
                                                
                                       ),
                                       ### end goal/team
                                       

                                       ### Minutes per Game
                                       tabPanel("Minutes per Game",
                                                br(),
                                                br(),

                                                plotlyOutput("playertime",height = "500px")
                                       ),
                                       ### end Minutes per Game
                                        

                                       ### radar for player
                                       tabPanel("Comprehensive Abilities",
                                                
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    # selectizeInput('year_player', 'Year',choices = goal$year, selected=""),
                                                    # selectizeInput('name', 'Player Name',choices = Prem$Name, selected=""),
                                                    
                                                  #check box of the polar chart
                                                  #   checkboxGroupInput('name', label = h3("Player Name"), 
                                                  #                      choices = Fulldata$Name[((Fulldata$Pos1=="Striker")|(Fulldata$Pos2=="Striker")|(Fulldata$Pos3=="Striker"))],
                                                  #                      selected ="")
                                                  # ),
                                                  # 
                                                  uiOutput("Selector")
                                                  ),
                                                  
                                                  # Show a plot of the generated distribution
                                                  mainPanel(

                                                    plotlyOutput("spider"),
                                                    p(
                                                      class = "text-muted",
                                                      paste("Note: Please select your players for display of radar charts."
                                                      )
                                                    )
              
                                                  )

                                                # sidebarLayout(
                                                #   sidebarPanel(
                                                # 
                                                #     #check box of the polar chart
                                                #     checkboxGroupInput('name', label = h3("Player Name"), 
                                                #                        choices = Prem$Name,
                                                #                        selected =""),
                                                # 
                                                #     width = 3
                                                #     
                                                #   ),
                                                #   
                                                #   mainPanel(
                                                #             plotOutput("spider", width = "100%", height = "800px")
                                                #   ),
                                                # 
                                                  # position = "right"
                                                # )
                                         )
                                         ### end radar for player
                                   
                                       )
                                   
                                    )### end tabset
                       
                    )  ### comparison
  ),
  
  
  
  ## Team tab
  tabPanel("Team",
           titlePanel("h2h Stats between Teams"),
           
           fluidRow(
             column(4,
               selectInput("Home",
                           label=" Select Home Team", 
                           choices=matches$Team,
                           selected = "Arsenal"
                          )),
           column(4,
               selectInput("Oppo",
                         label=" Select Opponent Team", 
                         choices=matches$Opponent,
                         selected = "Swansea City"
                         ))
               ),
           
           p(
             class = "text-muted",
             paste("Note: Please select your home team and opponent team for display of radar charts."
             )
           ),
           
           fluidRow(
             
             plotOutput("Spider"),
             tags$style(type="text/css",  ".shiny-output-error { visibility: hidden; }",  ".shiny-output-error:before { visibility: hidden; }")
             )
           )
          ## end team tab
)


