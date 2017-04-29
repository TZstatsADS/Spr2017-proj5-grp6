library(fmsb)
spider <- function(home,oppo,dat){
    #find the avd for both team
    dat <- na.omit(dat)
    home_game <- dat[dat$Team==home,]
    oppo_game <- dat[dat$Team==oppo,]
    home_stat <- dat[(dat$Team==home)&(dat$Opponent==oppo),]
    oppo_stat <- dat[(dat$Team==oppo)&(dat$Opponent==home),]
    home_oppo <- rbind(home_stat,oppo_stat)
    home_oppo <- aggregate(home_oppo[,c(7,20,10,34,16,32)],by=list(home_oppo$Team),FUN="mean")
    home_avg <- aggregate(home_game[,c(7,20,10,34,16,32)],by=list(home_game$Team),FUN="mean")
    oppo_avg <- aggregate(oppo_game[,c(7,20,10,34,16,32)],by=list(oppo_game$Team),FUN="mean")
    home_win <- aggregate(home_game[,c(7,20,10,34,16,32)],by=list(home_game$Result),FUN="mean")
    oppo_win <- aggregate(oppo_game[,c(7,20,10,34,16,32)],by=list(oppo_game$Result),FUN="mean")
    ho_max <- apply(dat[,c(24,7,20,10,34,16,32)],2,max)
    # ho_min <- apply(dat,2,0)
    ho_min <- rep(0,7)
    
    home_oppo <- rbind(ho_max,ho_min,home_oppo)
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
    home_oppo <- data.frame(apply(home_oppo,2,as.numeric))
    row.names(home_dat)<-c("max","min","avg","lose","tie","win")
    row.names(oppo_dat)<-c("max","min","avg","lose","tie","win")
    row.names(home_oppo)<-c("max","min","home","opponent")
    # home_chart <- radarchart(oppo_dat[,-1])
    colors_border=c( rgb(0.5,0.5,0.5,0.6), rgb(1,0.9,0.1,0.4) , rgb(0.5,0.8,0.1,0.4),rgb(0.9,0.5,0.5,0.5))
    colors_in=c( rgb(0.5,0.5,0.5,0.6), rgb(1,0.9,0.1,0.4) , rgb(0.5,0.8,0.1,0.4),rgb(0.9,0.5,0.5,0.5) )
    colors_border1=c( rgb(0.5,0.5,0.5,0.6), rgb(1,0.9,0.1,0.4) )
    colors_in1=c( rgb(0.5,0.5,0.5,0.6), rgb(1,0.9,0.1,0.4)  )
    
    par(mfrow = c(1,3))

    spider1=radarchart( home_dat[,-1]  , axistype=1 , title = "Home Team Historical Performance",
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=0.2,
    #custom labels
    vlcex=1.5
    )
    legend(x=0.6, y=1.2, legend = rownames(home_dat[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.5, pt.cex=2)
    
    spider2=radarchart( oppo_dat[,-1]  , axistype=1 , title = "Opponent Team Historical Performance", 
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=0.2,
    #custom labels
    vlcex=1.5
    )
    legend(x=0.6, y=1.2, legend = rownames(oppo_dat[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.5, pt.cex=2)
    
    spider3=radarchart( (home_oppo[,-1])  , axistype=1 , title = "h2h Historical Record",
    #custom polygon
    pcol=colors_border1 , pfcol=colors_in1 , plwd=1 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=0.2,
    #custom labels
    vlcex=1.5
    )
    legend(x=0.6, y=1.2, legend = rownames(home_oppo[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.5, pt.cex=2)
    spider1
    spider2
    spider3
}