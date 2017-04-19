clean_data <- function (file_i){
  file_i <- data.frame(file_i)
  file_i <- file_i[,-c(15:19,28)]
  # extract year and data
  file_i$Team <- rep(tail(names(sort(table(file_i$Home))),1),nrow(file_i))
  file_i$Year <- as.numeric(substr(file_i$Time,1,4))
  file_i$Month <- as.numeric(substr(file_i$Time,6,7))
  
  file_i$Homegame <- ifelse(file_i$Home==file_i$Team,1,0)
  file_i$Possession <-as.numeric(file_i$Possession)/100
  file_i$Opponent <- ifelse(file_i$Homegame==1,as.character(file_i$Away),as.character(file_i$Home))
  file_i$Goal <- ifelse(file_i$Homegame==1,substr(file_i$Score,1,1),substr(file_i$Score,3,3))
  file_i$Lose <- ifelse(file_i$Homegame==1,substr(file_i$Score,3,3),substr(file_i$Score,1,1))
  file_i$Result <- ifelse(file_i$Goal>file_i$Lose,"win",ifelse(file_i$Goal<file_i$Lose,"los","tie"))
  shot<-matrix(as.numeric(unlist(strsplit(gsub("[[:punct:]]"," ",file_i$Shots..OT.)," "))),ncol = 2,byrow = T)
  file_i$Shot <-shot[,1]
  file_i$Shot.Accu <-shot[,2]/shot[,1]
  pass <- matrix(as.numeric(unlist(strsplit(gsub("[[:punct:]]"," ",file_i$Passes..Success.)," "))),ncol = 2,byrow = T)
  file_i$Pass <- pass[,1]
  head <- matrix(as.numeric(unlist(strsplit(gsub("[[:punct:]]"," ",file_i$Head.Success.)," "))),ncol = 2,byrow = T)
  file_i$Head <-head[,1]
  file_i <- file_i[((file_i$Year>=2014)&(file_i$Month>9)),]
  return(file_i)
}

