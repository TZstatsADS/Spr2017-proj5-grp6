library(fmsb)
spider <- function(home,oppo,dat){
  #find the avd for both team
  dat <- na.omit(dat)
  home_game <- dat[dat$Team==home,]
  oppo_game <- dat[dat$Team==oppo,]
  home_avg <- aggregate(home_game[,c(7,20,10,34,16,32)],by=list(home_game$Team),FUN="mean")
  oppo_avg <- aggregate(oppo_game[,c(7,20,10,34,16,32)],by=list(oppo_game$Team),FUN="mean")
  home_win <- aggregate(home_game[,c(7,20,10,34,16,32)],by=list(home_game$Result),FUN="mean")
  oppo_win <- aggregate(oppo_game[,c(7,20,10,34,16,32)],by=list(oppo_game$Result),FUN="mean")
  home_dat <- rbind(home_avg,home_win)
  home_max <- apply(home_dat,2,max)
  home_min <- apply(home_dat,2,min)
  home_dat <- rbind(home_max,home_min,home_dat)
  oppo_dat <- rbind(oppo_avg,oppo_win)
  oppo_max <- apply(oppo_dat,2,max)
  oppo_min <- apply(oppo_dat,2,min)
  oppo_dat <- rbind(oppo_max,oppo_min,oppo_dat)
  home_dat <- data.frame(apply(home_dat,2,as.numeric))
  oppo_dat <- data.frame(apply(oppo_dat,2,as.numeric))
  row.names(home_dat)<-c("max","min","avg","lose","tie","win")
  row.names(oppo_dat)<-c("max","min","avg","lose","tie","win")
  home_chart <- radarchart(oppo_dat[,-1])
  colors_border=c( rgb(0.5,0.5,0.5,0.6), rgb(1,0.9,0.1,0.4) , rgb(0.5,0.8,0.1,0.4),rgb(0.9,0.5,0.5,0.5))
  colors_in=c( rgb(0.5,0.5,0.5,0.6), rgb(1,0.9,0.1,0.4) , rgb(0.5,0.8,0.1,0.4),rgb(0.9,0.5,0.5,0.5) )
  
  radarchart( home_dat[,-1]  , axistype=1 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.2,
              #custom labels
              vlcex=0.8 
  )
  legend(x=0.3, y=1.5, legend = rownames(home_dat[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1)
  
  radarchart( oppo_dat[,-1]  , axistype=1 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.2,
              #custom labels
              vlcex=0.8 
  )
  legend(x=0.3, y=1.5, legend = rownames(oppo_dat[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1)
}
