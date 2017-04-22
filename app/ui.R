library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Player Goals and Time"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput('year_player', 'Year',choices = goal$year, selected="")
#      selectizeInput('year_time', 'Year',choices = goal$year, selected="")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("playergoal"),
       br(),
       br(),
       plotlyOutput("palyertime")
       
    )
  )
))
