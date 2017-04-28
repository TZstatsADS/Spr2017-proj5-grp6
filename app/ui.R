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
matches <- read.csv("../data/matches.csv")

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
                                          
                                          selectInput("season",
                                                      label = "Select the Season", 
                                                      choices = c("2015","2016"), selected="2016")),
                                      
                                          selectInput("Position",
                                                      label="Start searching by position", 
                                                      choices=unique(Prem$Pos1),
                                                      selected = "ST"),  #choose player position
                            
                                          
                            
                                          selectInput("tag",
                                                      label=" Select Player by play style", 
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
                      
                      titlePanel("Comparison for selected players"),
                      
                                 sidebarLayout(
                                   sidebarPanel(

                                       selectizeInput('year_player', 'Year',choices = goal$year, selected=""),
                                       selectizeInput('name', 'Player Name',choices = Prem$Name, selected=""),
   
                                      #check box of the polar chart
                                      checkboxGroupInput('name', label = h3("Player Name"), 
                                                          choices = Prem$Name,
                                                          selected =""),

                                     
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     
                                     tabsetPanel(
                                       
                                       ### 6-dim
                                       tabPanel("scorer, passing, defensive",
                                                titlePanel("scorer, passing, defensive"),

                                                fluidRow(
                                                    column(2,  selectInput(
                                                          "stats1",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Goals.P")  #choose player position
                                                    ),
                                                    column(2,  selectInput(
                                                          "stats2",label="x axis stats", choices=colnames(Prem_player)[c(3,4:10)],selected = "Shots")  #choose player position
                                                    )),
                                                    
                                                fluidRow( 
                                                    column(9,plotlyOutput("plotlydef")
                                                    )),
                                                
                                                fluidRow(    
                                                    column(2,  selectInput(
                                                          "stats3",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Tackles")  #choose player position
                                                    ),
                                                    column(2,  selectInput(
                                                          "stats4",label="x axis stats", choices=colnames(Prem_player)[c(15:23)],selected = "Fouls")  #choose player position
                                                    )),
                                                
                                                fluidRow(
                                                    column(9,plotlyOutput("plotlypass")
                                                    )),
                                                
                                                fluidRow(
                                                    column(2,  selectInput(
                                                          "stats5",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Pass.Success")  #choose player position
                                                    ),
                                                    column(2,  selectInput(
                                                          "stats6",label="x axis stats", choices=colnames(Prem_player)[c(24:32)],selected = "Passes")  #choose player position
                                                    )),
                                                fluidRow(    
                                                    column(10,offset=0.6,plotlyOutput("plotlyscore")
                                                    ))
                                                
                                                
                                                #plotlyOutput("plotlydef"),
                                                #plotlyOutput("plotlypass"),
                                                #plotlyOutput("plotlyscore")                          
                                     
                                       ),
                                       
                                       ### goal/team
                                       tabPanel("Goal/Team",
                                                titlePanel("Goal/Team"),

                                                plotlyOutput("playergoal")
                                                
                                       ),
                                       
                                       ###avg.
                                       tabPanel("Minutes per Game",
                                                titlePanel("avg."),

                                                plotlyOutput("playertime")
                                       ),
                                                
                                       ### radar
                                       tabPanel("radar",
                                                titlePanel("radar"),

                                                plotOutput("spider")
                                       )
                                       
                                   
                                   
                                       )
                                     
                                 
                                 
                                    ),
                                   
                                   position = "right"
                                    ### comparison
                                )
                      
                      
             
             
                       
  )
  ),
  
  ## Team tab
  tabPanel("Team",
           titlePanel("Team Comparison")
           
           
           
           
  )
  
  ## end team tab
)


