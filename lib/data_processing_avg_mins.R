load("../output/Prem_player.RData")

Prem_player$Play.Sub. <- gsub("\\((.*)\\)","",Prem_player$Play.Sub.)
Prem_player$Play.Sub. <- as.numeric(Prem_player$Play.Sub.)
avg_mins <- Prem_player$Mins / Prem_player$Play.Sub.
Prem_player <- mutate(Prem_player, avg_mins)

save(Prem_player,file = "Prem_player.RData")
