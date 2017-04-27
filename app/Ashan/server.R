library(shiny)
library(plotly)
library(RColorBrewer)
library(fmsb)

shinyServer(function(input, output) {
   
  year1 <- reactive({
    input$year_player
  })
  
  # similar
  # Pcluster <- reactive({
  # input$cluster
  #})
  
  Pname <- reactive({
    input$name
  })
  
#first chart: playergoal  
  output$playergoal<-renderPlotly({
    temp <- subset(goal, 
                   (year == year1())
                #add one more cluster filter 
                #cluster == Pcluster()
                   )
    temp %>%
      plot_ly(x = ~team) %>%
      add_trace(y = ~goals, type="scatter", marker = list(color = "red", size = 10), text = ~player,
                mode = "markers") %>%
      layout(
        title = "Player goals by team",
        xaxis = list(title = "", tickfont = list(size = 10))
      )
  })

#second chart: playertime  
  output$palyertime<-renderPlotly({
    temp <- subset(mins, 
                   (year == year1())
                   #add one more cluster filter
                   #cluster == Pcluster()
                   )
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
  

#third chart: polar chart      
  output$spider<-renderPlotly({
    temp_radar <- select(Prem, Name, #Ball_Control, 
                         Aggression, Acceleration,
                         Finishing, Jumping, Heading)
    
    temp_name <- subset(temp_radar, (Name == Pname()))
    temp_show <- select(temp_name, #Ball_Control, 
                        Aggression, Acceleration,
                        Finishing, Jumping, Heading)
    tdshow <- gather(temp_show)
    
    tdshow %>%
      plot_ly(r = ~value, t = ~key) %>%
      add_area() %>%
      layout(
        radialaxis = list(ticksuffix = "%", range = c(0, 100), showticklabels = TRUE),
        angularaxis = list(showticklabels = TRUE,
                           tickorientation = 'horizontal', ticklen = 6, visible = TRUE),
        xaxis = list(
          showgrid = FALSE, 
          showline = FALSE, 
          showticklabels = FALSE, 
          zeroline = FALSE
        ),
        yaxis = list(
          showgrid = FALSE, 
          showline = FALSE, 
          showticklabels = FALSE, 
          zeroline = FALSE
        ),
        showlegend = FALSE)
        
})
 
  
})


