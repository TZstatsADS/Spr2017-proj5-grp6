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
# goal <- select(score, Player, Team, Goals, Year)
# names(goal) <- c("player", "team", "goals", "year")
# mins <- select(score, Player, Team, Play, Mins, Avg_mins, Year)
# names(mins) <- c("player", "team", "play", "mins", "avg_mins", "year")


### data
load("../output/FIFAFull.RData")
load("../output/FIFAPrem.RData")
load("../output/Player_stats.RData")
load("../output/Fulldata15.RData")
load("../output/Fulldata16.RData")
load("../output/Fulldata.RData")
load("../output/Prem_player.RData")
matches <- read.csv("../data/matches.csv")



server = function(input, output) {
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Soccer Manager is a powerful tool for player selection. blablabla<br/>
         between the United States and the rest of the world<br/><br/><br/><br/>Group 6: Jiahao, Xiaowo, Zhengyuan, Zhishan")
  })
  
  
  observe({
      dataInput<-reactive({
        Fulldata[((Fulldata$Pos1==input$Position)|(Fulldata$Pos2==input$Position)|(Fulldata$Pos3==input$Position))&(Fulldata$Year==input$season),]
          # selected15 <- Fulldata15[(Fulldata15$Pos1==input$Position)|(Fulldata15$Pos2==input$Position)|(Fulldata15$Pos3==input$Position),]
          # selected16 <- Fulldata16[(Fulldata16$Pos1==input$Position)|(Fulldata16$Pos2==input$Position)|(Fulldata16$Pos3==input$Position),]
      })
      
      dataInput1<-reactive({
          set.seed(1)
          kmeans(dataInput()[, 19:49], 3, nstart = 20)
      })
      
      output$barPlot <- renderPlotly({
        # k means cluster
      
        # choose the numeric variables to do pca
        
        if(input$Position=="GK") {
          dat <- dataInput()[, 50:54]}
        else{dat <- dataInput()[, 19:49]}
        
        pc <- princomp(dat, cor = TRUE, scores=TRUE)
        
        #prepare dat1 for 3-dimensional visualization
        dat1 <- data.frame(cbind(pc$scores[,1:3],dataInput1()$cluster))
        
        dat2<-data.frame(cbind(dataInput()[,2],dataInput()[,6]))
        dat1<-data.frame(cbind(dat1,dat2))
        
        #clusplot(dat,KCluster_CAM$cluster,color = T,shade = T)
        
        plot_ly(dat1, 
                x = dat1[,1], 
                y = dat1[,2], 
                z = dat1[,3], 
                text=paste(dat1$X1,"\t",dat1$X2),
                type = "scatter3d", 
                mode = "markers", 
                color=factor(dat1$V4),
                hoverinfo="text"
        )
          
      })
      
      output$table1 <- renderDataTable({
        #rows of a position
        load("../output/pos_table.RData")
        table1<-pos_table[pos_table$Position==input$Position,c(3,4)]
        datatable(table1, options=list(searching = F,lengthChange=F,paging=T), rownames=F)
      })
      
      
      output$table <- renderDataTable({

        dat3<-data.frame(cbind(dataInput(),t=dataInput1()$cluster))
        table1<-dat3[which(input$tag==dat3$t),c(2,3,6,9:15)]
        datatable(table1, options=list(searching = T,lengthChange=F,paging=T), rownames=F)
      })
      
      
      output$selection <- renderPrint({
        # click to select a row
        s1 = input$table_rows_selected
        # s2 <- event_data("plotly_click")
        # 
        dat3<-data.frame(cbind(dataInput(),t=dataInput1()$cluster))
        table1<-dat3[which(input$tag==dat3$t),c(2,3,6,9:15)]
        if (length(s1)) {
          cat('These rows were selected:\n\n')
          cat(table1[s1,1], sep = '\n')
        }
         # if(length(s2)){
         #   dat3[s2[[x]],]
         # }

      })
      
      output$plotlydef <- renderPlotly({
        namelist <- dataInput()$Name[dataInput1()$cluster==input$tag]
        d <- Prem_player[Prem_player$n1name%in%namelist,]
        # par(mfrow=c(1,2))
        {
        p <- plot_ly(
          d, x = ~as.numeric(d[,input$stats2]), y = ~as.numeric(d[,input$stats1]),
          showlegend=FALSE,
          # Hover text:
          text = ~paste("Name: ",Player, '<br>Team:',Team),
          color = ~Team
          )
         # p2 <- plot_ly(
         #    d, x = ~Tackles, y = ~Fouled,
         #    # Hover text:
         #    text = ~paste("Name: ",Player, '<br>Team:',Team),
         #    color = ~Team
         #  )
          
        }

      })
      
      output$plotlypass <- renderPlotly({  
        s <- event_data("plotly_click", source = "plotlydef")
        namelist <- dataInput()$Name[dataInput1()$cluster==input$tag]
        d <- Prem_player[Prem_player$n1name%in%namelist,]
        # par(mfrow=c(1,3))
        p <- plot_ly(
          d, x = ~as.numeric(d[,input$stats4]), y = ~as.numeric(d[,input$stats3]),
          showlegend=FALSE,
          # Hover text:
          text = ~paste("Name: ",Player, '<br>Team:',Team),
          color = ~Team
          ) 

      })
      
      output$plotlyscore <- renderPlotly({
        s <- event_data("plotly_click", source = "plotlydef")
        namelist <- dataInput()$Name[dataInput1()$cluster==input$tag]
        d <- Prem_player[Prem_player$n1name%in%namelist,]

        p <- plot_ly(
          d, x = ~as.numeric(d[,input$stats6]), y = ~as.numeric(d[,input$stats5]),
          #showlegend=FALSE,
          # Hover text:
          text = ~paste("Name: ",Player, '<br>Team:',Team),
          color = ~Team
        )

      })


  })
    
    
    ### ashan
    
    #year1 <- reactive({
    #  input$year_player
    #})
    
    # similar
    # Pcluster <- reactive({
    # input$cluster
    #})
    
    #Pname <- reactive({
    #  input$name
    #})
    
    #first chart: playergoal  
    output$playergoal<-renderPlotly({
      #temp <- subset(goal, 
      #               (year == year1())
                     #add one more cluster filter 
                     #cluster == Pcluster()

      namelist <- dataInput()$Name[dataInput1()$cluster==input$tag]
      temp <- Prem_player[Prem_player$n1name%in%namelist,]
      
      temp %>%
        plot_ly(x = ~Team) %>%
        add_trace(y = ~Goal, type="scatter", marker = list(color = "red", size = 10), text = ~Player,
                  mode = "markers") %>%
        layout(
          title = "Player goals by team",
          xaxis = list(title = "", tickfont = list(size = 10))
        )
        
      
    })
    

    #year1 <- reactive({
    #  input$year_player
    #})
    
    # similar
    # Pcluster <- reactive({
    # input$cluster
    #})
    
    #Pname <- reactive({
    #  input$name
    #})
    
    #second chart: playertime  
    output$playertime<-renderPlotly({
      #temp <- subset(mins, 
                     #(year == year1())
                     #add one more cluster filter
                     #cluster == Pcluster()

      namelist <- dataInput()$Name[dataInput1()$cluster==input$tag]
      temp <- Prem_player[Prem_player$n1name%in%namelist,]

      
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
    
    #year1 <- reactive({
    #  input$year_player
    #})
    
    # similar
    # Pcluster <- reactive({
    # input$cluster
    #})
    
    #Pname <- reactive({
    #  input$name
    #})
    

      #namelist <- dataInput()$Name[dataInput1()$cluster==input$tag]
      #temp <- Prem_player[Prem_player$n1name%in%namelist,]



    output$spider<-renderPlotly({
      temp_radar <- select(Prem, Name, #Ball_Control, 
                           Aggression, Acceleration,
                           Finishing, Jumping, Heading)
      #change temp_name when merge
      temp_name <- subset(temp_radar, (Name %in% Pname()))
      # temp_show <- select(temp_name,  #Ball_Control, 
      #                     Aggression, Acceleration,
      #                     Finishing, Jumping, Heading)
      tdshow <- gather(temp_name, Ability, Score ,-Name)
      
      tdshow %>%
        plot_ly(r = ~Score, t = ~Ability) %>%
        add_area(color = ~Name) %>%
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
          showlegend = TRUE,
          legend = list(orientation = 'h'))
      
    })
    
    
    
  }
