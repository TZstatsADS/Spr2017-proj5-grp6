library(shinythemes)
library(shiny)
library(DT)
library(plotly)

load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/FIFAFull.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/FIFAPrem.RData")
load("/Users/apple/Documents/R/Spr2017-proj5-grp6/output/Player_stats.RData")
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
                 column(6,  selectInput(
                   "Position",label="Start searching by position", choices=unique(Prem$Pos1))  #choose player position
                 ),
                 column(6, selectInput("tag",label=" Select Player by play style", choices=c(1,2,3))#orig_1617$CUISINE.DESCRIPTION
                        
                 )),
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
                               DT::dataTableOutput("table"))
               )         
      ),    

      tabPanel("Know your Opponents"
              
               )
               )    
      
    ),
  
  
  
  
  
  
#server
  server = function(input, output) {
    observe({
      output$barPlot <- renderPlotly({
        selected <- Prem[(Prem$Pos1==input$Position)|(Prem$Pos2==input$Position)|(Prem$Pos3==input$Position),]
        # k means cluster
        set.seed(1)
        KCluster <- kmeans(selected[, 19:49], 3, nstart = 20)
        
        # choose the numeric variables to do pca

        
        if(input$Position=="GK") {
          dat <- selected[, 50:54]}
        else{dat <- selected[, 19:49]}
        pc <- princomp(dat, cor = TRUE, scores=TRUE)
        
        #prepare dat1 for 3-dimensional visualization
        dat1 <- data.frame(cbind(pc$scores[,1:3],KCluster$cluster))
        dat2<-data.frame(cbind(selected[,2],selected[,6]))
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
        #rows of a position
        selected <- Prem[(Prem$Pos1==input$Position)|(Prem$Pos2==input$Position)|(Prem$Pos3==input$Position),]
        # k means cluster
        set.seed(1)
        KCluster <- kmeans(selected[, 19:49], 3, nstart = 20)
        dat3<-data.frame(cbind(selected,t=KCluster$cluster))
        table1<-dat3[which(input$tag==dat3$t),c(2,3,6,9:15)]
        datatable(table1, options=list(searching = T,lengthChange=F,paging=T), rownames=F)
      })
      
      
      output$selection <- renderPrint({
        # click to select a row
        s1 = input$table_rows_selected
        s2 <- event_data("plotly_click")
        selected <- Prem[(Prem$Pos1==input$Position)|(Prem$Pos2==input$Position)|(Prem$Pos3==input$Position),]
        # k means cluster
        KCluster <- kmeans(selected[, 19:49], 3, nstart = 20)
        dat3<-data.frame(cbind(selected,t=KCluster$cluster))
        table1<-dat3[which(input$tag==dat3$t),c(2,3,6,9:15)]
        if (length(s1)) {
          cat('These rows were selected:\n\n')
          cat(table1[s1,1], sep = ', ')
        }
        # if(length(s2)){
        #   dat3[s2[[x]],]
        # }

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