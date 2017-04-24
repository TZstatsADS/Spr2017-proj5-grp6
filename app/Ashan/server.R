library(shiny)
library(plotly)
library(RColorBrewer)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  year1 <- reactive({
    input$year_player
  })
  
  # year2 <- reactive({
  #   input$year_time
  # }) 
  
  output$playergoal<-renderPlotly({
    temp <- subset(goal, 
                   (year == year1()))
    temp %>%
      plot_ly(x = ~team) %>%
      add_trace(y = ~goals, type="scatter", marker = list(color = "red", size = 10), text = ~player,
                mode = "markers") %>%
      layout(
        title = "Player goals by team",
        xaxis = list(title = "", tickfont = list(size = 10))
      )
  })
  

  
  
  
  output$palyertime<-renderPlotly({
    temp <- subset(mins, 
                   (year == year1()))
    colourCount = length(unique(temp$team))
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    cols = getPalette(colourCount)
    temp$color <- factor(temp$team, labels = cols)
    temp %>%
      plot_ly(x = ~play) %>%
      add_trace(y = ~avg_mins, type="scatter", marker = list(color = ~color, size = 10), text = ~paste(team, player, sep = ' : '),
                mode = "markers") %>%
      layout(
        title = "Player average time",
        xaxis = list(title = "", tickfont = list(size = 10))
      )
  })
  
})
