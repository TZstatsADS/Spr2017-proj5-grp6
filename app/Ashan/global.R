library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
score <- read.csv("../data/scorer2_2015-17.csv")
goal <- select(score, Player, Team, Goals, Year)
names(goal) <- c("player", "team", "goals", "year")
mins <- select(score, Player, Team, Play, Mins, Avg_mins, Year)
names(mins) <- c("player", "team", "play", "mins", "avg_mins", "year")

#RADAR CHART
load("../output/FIFAFull.RData")
load("../output/FIFAPrem.RData")

