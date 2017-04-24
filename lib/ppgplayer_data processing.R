####### Data Process for ppg #######

library(stringr)
library(dplyr)

ppgdata <- read.csv("../data/score_pass_defense/scorer_2015-17.csv")
#ppgdata$Play..Sub.

#### For Play
## extract numbers of nonstarter from "()" and add to the column
gsub(".*\\((.*)\\).*", "\\1",ppgdata$Play..Sub.)
Nons <- str_match(ppgdata$Play..Sub., "\\((.*)\\)")[,2]
Nons[is.na(Nons)] <- 0
Nons <- as.numeric(Nons)
ppgdata <- mutate(ppgdata, Nons)

## remove "()" 
Total <- gsub("\\((.*)\\)","",ppgdata$Play..Sub.)
Total <- as.numeric(Total)
ppgdata <- mutate(ppgdata, Total)

## calculate starter
Start <- ppgdata$Total - ppgdata$Nons
ppgdata <- mutate(ppgdata, Start)


#### For Goal
## extract numbers of P from "()" and add to the column
gsub(".*\\((.*)\\).*", "\\1",ppgdata$Goals.P.)
P <- str_match(ppgdata$Goals.P., "\\((.*)\\)")[,2]
P[is.na(P)] <- 0
P <- as.numeric(P)
ppgdata <- mutate(ppgdata, P)

## remove "()" 
Total_goal <- gsub("\\((.*)\\)","",ppgdata$Goals.P.)
Total_goal <- as.numeric(Total_goal)
ppgdata <- mutate(ppgdata, Total_goal)

## calculate nonp
non_P <- ppgdata$Total_goal - ppgdata$P
ppgdata <- mutate(ppgdata, non_P)
ppgdata <- ppgdata[,-c(16,17,18)]
ppgdata
write.csv(ppgdata, "../data/score_pass_defense/ppgdata.csv")

####### Plot Start Here #######


