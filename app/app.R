library(shinythemes)
library(shiny)
library(DT)
library(plotly)

load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/FIFAFull.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/FIFAPrem.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/Player_stats.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/Fulldata15.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/Fulldata16.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/Fulldata.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/Prem_player.RData")

shinyApp(
# UI
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      #theme = "slat",  # <--- To use a theme, uncomment this
      "Soccer Manager",
      tabPanel("HOME"),
      tabPanel("Shop your players",
               fluidRow(                           
                 column(4,  selectInput(
                   "Position",label="Start searching by position", choices=unique(Prem$Pos1),selected = "ST")  #choose player position
                 ),
                 column(4, selectInput("tag",label=" Select Player by play style", choices=c(1,2,3))#orig_1617$CUISINE.DESCRIPTION
                        
                 ),
                 column(4,selectInput("season",label = "Select the Season", choices = c("2015","2016"), selected="2016"))
                 ),
               br(),
               br(),
               fluidRow(
                 column(6,offset = 0,
                        plotlyOutput("barPlot")),
                 column(6,offset = 0.6,
                        DT::dataTableOutput("table1")),
                 
                 column(6,
                        verbatimTextOutput("selection")
                        ),
                 
                 column(10,offset=0.6,
                               DT::dataTableOutput("table")
                        )
               ),
               br(),
               br(),
               fluidRow(
                 column(2,  selectInput(
                   "stats1",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Goals.P")  #choose player position
                 ),
                 column(2,  selectInput(
                   "stats2",label="x axis stats", choices=colnames(Prem_player)[c(3,4:10)],selected = "Shots")  #choose player position
                 ),
                 column(2,  selectInput(
                   "stats3",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Tackles")  #choose player position
                 ),
                 column(2,  selectInput(
                   "stats4",label="x axis stats", choices=colnames(Prem_player)[c(15:23)],selected = "Fouls")  #choose player position
                 ),
                 column(2,  selectInput(
                   "stats5",label="y axis stats", choices=colnames(Prem_player)[c(3,4:10,15:31)],selected = "Pass.Success")  #choose player position
                 ),
                 column(2,  selectInput(
                   "stats6",label="x axis stats", choices=colnames(Prem_player)[c(24:32)],selected = "Passes")  #choose player position
                 ),
                 column(3,plotlyOutput("plotlydef")
                 ),
                 column(3,plotlyOutput("plotlypass")
                 )
                 ,
                 column(6,offset=0.6,plotlyOutput("plotlyscore")
                 )
               ) 
      ),    

      tabPanel("Know your Opponents"
              
               )
               )    
      
    ),
  
  
  
  
  
  
#server
  server = function(input, output) {
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
        load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/pos_table.RData")
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

      # output$scatterplot <- renderPlotly({
      #   s <- event_data("plotly_click", source = "heatplot")
      #   if (length(s)) {
      #     vars <- c(s[["x"]], s[["y"]])
      #     d <- setNames(mtcars[vars], c("x", "y"))
      #     yhat <- fitted(lm(y ~ x, data = d))
      #     plot_ly(d, x = ~x) %>%
      #       add_markers(y = ~y) %>%
      #       add_lines(y = ~yhat) %>%
      #       layout(xaxis = list(title = s[["x"]]),
      #              yaxis = list(title = s[["y"]]),
      #              showlegend = FALSE)
      #   } else {
      #     plotly_empty()
      #   }
      # })
  })
 }
)