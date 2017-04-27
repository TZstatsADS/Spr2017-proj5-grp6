library(shiny)
library(plotly)

shinyUI(fluidPage(
  
 
  titlePanel("Player Goals and Time"),
  
   
  sidebarLayout(
    sidebarPanel(
      selectizeInput('year_player', 'Year',choices = goal$year, selected=""),
      # selectizeInput('name', 'Player Name',choices = Prem$Name, selected=""),
   
    #check box of the polar chart
     checkboxGroupInput('name', label = h3("Player Name"), 
                        choices = Prem$Name,
                        selected ="")
     ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("playergoal"),
       br(),
       br(),
       plotlyOutput("playertime"),
       plotlyOutput("spider")


    )
  )
))
