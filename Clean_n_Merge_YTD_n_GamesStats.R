library(rvest)
library(data.table)
library(stringr)
library(dplyr)

source('Get_Team_Stats_YTD.R')
source('Get_Game_Outcomes.R')
GetNFLTeamStatsYTD(2020)


GamesScores <- GetGameScores(8)

#### GAMES DATASET ####
# 1 Observation = 1 game from 1 teams' perspective

# Required Columns:
  # Team , Opponent, Team_Score, Opp_Score, Outcome, Score_Difference

## 1) Duplicate Games ü: 1 Observation = 1 Game for 1 Team
GamesDup <- GamesScores 

GamesDup$Team <- GamesDup$Away_Team
GamesDup$Opponent <- GamesDup$Home_Team
GamesDup$Played_at <- "Away"

GamesDup1 <- GamesScores 

GamesDup1$Team <- GamesDup1$Home_Team
GamesDup1$Opponent <- GamesDup1$Away_Team
GamesDup1$Played_at <- "Home"

Games <- rbind(GamesDup,GamesDup1) %>% select(-c(Away_Team,Home_Team))

# Remove Unneeded Tables
rm(list= c("GamesDup","GamesDup1","GamesScores"))

## 2) Derive Team & Opponent points using winning_team, Points Winner/Loser
for (i in 1:(length(Games$Winning_Team))) {
  if (Games$Winning_Team[i] == Games$Team[i] ) {
    Games$Outcome[i] <- "Win"
    Games$Team_Score[i] <- Games$Points_Winner[i]
    Games$Opp_Score[i] <- Games$Points_Loser[i]
    
  } else {
    Games$Outcome[i] <- "Loss"
    Games$Team_Score[i] <- Games$Points_Loser[i]
    Games$Opp_Score[i] <- Games$Points_Winner[i]
  }
  
}

Games <- Games %>% select(Team,Opponent, Played_at,Team_Score, Opp_Score, Outcome ) %>% 
  mutate(Difference = Team_Score - Opp_Score)

## 3) Transform Team Names to for Joins
  TeamNames <- unlist(lapply(Games$Team,function(x) {
    x <- gsub(".NYJ"," Jets", x)
    x <- gsub(".NYG"," Giants", x)
    x <- gsub(".LAC"," Chargers", x)
    x <- gsub(".LAR"," Rams", x)
  }))
Games$Team <- TeamNames

# GameTeams <- unique(GamesWk8[1])

for (i in 1:length(Games$Team)) {
  if (paste0(unlist(str_split(Games$Team[i]," "))[1]
             ," ",
             unlist(str_split(Games$Team[i]," "))[2]) == "New York" || 
      paste0(unlist(str_split(Games$Team[i]," "))[1]
             ," ",
             unlist(str_split(Games$Team[i]," "))[2]) == "Los Angeles") {
    next()
  } else if ((length(unlist(str_split(Games$Team[i]," ")))) == 3) {
    
    Games$Team[i] <- paste0(unlist(str_split(Games$Team[i]," "))[1]
                             ," ",
                             unlist(str_split(Games$Team[i]," "))[2])
  } else {
    Games$Team[i] <- paste0(unlist(str_split(Games$Team[i]," ")))[1]
  }
  next()
}


## 4) Transform Opponent Names to for Joins
  OppNames <- unlist(lapply(Games$Opponent,function(x) {
    x <- gsub(".NYJ"," Jets", x)
    x <- gsub(".NYG"," Giants", x)
    x <- gsub(".LAC"," Chargers", x)
    x <- gsub(".LAR"," Rams", x)
  }))
Games$Opponent <- OppNames
  


for (i in 1:length(Games$Opponent)) {
  if (paste0(unlist(str_split(Games$Opponent[i]," "))[1]
             ," ",
             unlist(str_split(Games$Opponent[i]," "))[2]) == "New York" || 
      paste0(unlist(str_split(Games$Opponent[i]," "))[1]
             ," ",
             unlist(str_split(Games$Opponent[i]," "))[2]) == "Los Angeles") {
    next()
    
  } else if ((length(unlist(str_split(Games$Opponent[i]," ")))) == 3) {
    
    Games$Opponent[i] <- paste0(unlist(str_split(Games$Opponent[i]," "))[1]
                            ," ",
                            unlist(str_split(Games$Opponent[i]," "))[2])
  } else {
    Games$Opponent[i] <- paste0(unlist(str_split(Games$Opponent[i]," ")))[1]
  }
  next()
}





#### YEAR-TO-DATE TEAM STATISTICS ####
  YTDTeams <- GetNFLTeamStatsYTD(2020)
  # colnames(YTDTeams)

  
# Current Variables: -> per Offense / Defense
  # Passing (12):
    # CMP   = Completions
    # ATT   = Attempts
    # CMP%  = Completion %
    # YDS   = Total Yards
    # AVG   = Yards / Attempts (per Passing Play)
    # YDS/G = Yards / Game
    # LNG   = Longest Passing Play YTD
    # TD    = Touchdowms
    # INT   = Interceptions
    # SACK  = Sacks
    # SYL   = Sack Yards Lost
    # RTG   = Quarterback Rating
  
  # Rushing (20):
    # ATT = Attempts
    # YDS = Total Yards 
    # AVG = Yards / Attempt
    # YDS/G = Yards / Game
    # LNG = Longest Rushing Play YTD
    # TD = Touchdowns
    # FUM = Fumbles
    # LST = Fumbles Lost
  

  
# Correct YTD Stats Team Names to Join with Game Stats 
for (i in 1:length(YTDTeams$Team)) {
  if (paste0(unlist(str_split(YTDTeams$Team[i]," "))[1]
             ," ",
             unlist(str_split(YTDTeams$Team[i]," "))[2]) == "New York" || 
      paste0(unlist(str_split(YTDTeams$Team[i]," "))[1]) == "Los") {
    
    next()
  } else  if ((length(unlist(str_split(YTDTeams$Team[i]," ")))) == 3) {
    
    YTDTeams$Team[i] <- paste0(unlist(str_split(YTDTeams$Team[i]," "))[1]
                            ," ",
                            unlist(str_split(YTDTeams$Team[i]," "))[2])
  } else {
    YTDTeams$Team[i] <- paste0(unlist(str_split(YTDTeams$Team[i]," ")))[1]
  }
  next()
}

colnames(YTDTeams) <- gsub("/","_pr_",colnames(YTDTeams))
colnames(YTDTeams) <- gsub("%","_prc",colnames(YTDTeams))

# Keep only: 
  # Passing/Rushing Plays Attended, Completion%, YDS/G, Touchdowns
  # ,Interception / Fumbles lost, Sacks, QBR  
  # Both from Offense / Defense

YTDTeams <- YTDTeams %>% mutate(off__rush_FUM = off_rush_LST,
                                def__rush_FUM = def_rush_LST) %>% 
  select(Team, GP   ,off_pass_ATT ,off_pass_CMP_prc   ,off_pass_YDS_pr_G
                    ,off_pass_TD  ,off_pass_INT       ,off_pass_SACK     ,off_pass_RTG
                    ,off_rush_ATT ,off_rush_YDS_pr_G  ,off_rush_TD       ,off__rush_FUM
                    ,def_pass_ATT ,def_pass_CMP_prc   ,def_pass_YDS_pr_G
                    ,def_pass_TD  ,def_pass_INT       ,def_pass_SACK     ,def_pass_RTG
                    ,def_rush_ATT ,def_rush_YDS_pr_G  ,def_rush_TD       ,def__rush_FUM) 




#### MERGE GAMES & YTD STATS TABLES ####


GameswTeam <- Games %>% left_join(YTDTeams, by = c("Team" = "Team"), suffix = c(".",".Team_"))

Headers <- colnames(GameswTeam)
Headers[9:30] <- paste0("Team_", Headers[9:30])
colnames(GameswTeam) <- Headers

GameswOpp <- GameswTeam %>% left_join(YTDTeams, by = c("Opponent" ="Team"), suffix = c("_Team","_Opp"))
Headers <- colnames(GameswOpp)
Headers[32:53] <- paste0("Opp_", Headers[32:53])
colnames(GameswOpp) <- Headers

Dataset <- GameswOpp

rm(list = c("Games","Gamestest","GamestestOpp","GameswOpp","GameswTeam","YTDTeams"
            ,"YTDTest","Headers","i","JointHeaderTeam","OppNames","TeamHeaders","TeamNames"))





View(Dataset)
