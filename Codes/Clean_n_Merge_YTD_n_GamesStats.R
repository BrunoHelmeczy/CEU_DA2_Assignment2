library(rvest)
library(data.table)
library(stringr)
library(dplyr)
library(tidyverse)

source('Codes/Get_Team_Stats_YTD.R')
source('Codes/Get_Game_Outcomes.R')
# GetNFLTeamStatsYTD(2020)


GamesScores <- GetGameScores(15)

#### GAMES DATASET ####
# 1 Observation = 1 game 

# Required Columns:
  # Team , Opponent, Team_Score, Opp_Score, Outcome, Score_Difference

## 1) Duplicate Games ü: 1 Observation = 1 Game for 1 Team
# GamesDup <- GamesScores 

GamesScores$Team <- GamesScores$Away_Team
GamesScores$Opponent <- GamesScores$Home_Team
GamesScores$Played_at <- "Opponent"

#GamesDup1 <- GamesScores 

#GamesDup1$Team <- GamesDup1$Home_Team
#GamesDup1$Opponent <- GamesDup1$Away_Team
#GamesDup1$Played_at <- "Home"

# Games <- rbind(GamesDup,GamesDup1) %>% select(-c(Away_Team,Home_Team))

# Remove Unneeded Tables
# rm(list= c("GamesDup","GamesDup1","GamesScores"))




## 2) Derive Team & Opponent points using winning_team, Points Winner/Loser
for (i in 1:(length(GamesScores$Winning_Team))) {
  if (GamesScores$Winning_Team[i] == GamesScores$Team[i] ) {
    GamesScores$Team_Outcome[i] <- "Win"
    GamesScores$Team_Score[i] <- GamesScores$Points_Winner[i]
    GamesScores$Opp_Score[i]  <- GamesScores$Points_Loser[i]
    
  } else {
    GamesScores$Team_Outcome[i] <- "Loss"
    GamesScores$Team_Score[i] <- GamesScores$Points_Loser[i]
    GamesScores$Opp_Score[i]  <- GamesScores$Points_Winner[i]
  }
  
}

GamesScores <- GamesScores %>% dplyr::select(Team, Opponent, Team_Outcome, Team_Score, Opp_Score, Winner, Week  ) %>% 
  mutate(Difference = Team_Score - Opp_Score)

## 3) Transform Team Names to for Joins
  TeamNames <- unlist(lapply(GamesScores$Team,function(x) {
    x <- gsub(".NYJ"," Jets", x)
    x <- gsub(".NYG"," Giants", x)
    x <- gsub(".LAC"," Chargers", x)
    x <- gsub(".LAR"," Rams", x)
  }))
  GamesScores$Team <- TeamNames


for (i in 1:length(GamesScores$Team)) {
  if (paste0(unlist(str_split(GamesScores$Team[i]," "))[1]
             ," ",
             unlist(str_split(GamesScores$Team[i]," "))[2]) == "New York" || 
      paste0(unlist(str_split(GamesScores$Team[i]," "))[1]
             ," ",
             unlist(str_split(GamesScores$Team[i]," "))[2]) == "Los Angeles") {
    next()
  } else if ((length(unlist(str_split(GamesScores$Team[i]," ")))) == 3) {
    
    GamesScores$Team[i] <- paste0(unlist(str_split(GamesScores$Team[i]," "))[1]
                             ," ",
                             unlist(str_split(GamesScores$Team[i]," "))[2])
  } else {
    GamesScores$Team[i] <- paste0(unlist(str_split(GamesScores$Team[i]," ")))[1]
  }
  next()
}


## 4) Transform Opponent Names too for Joins
  OppNames <- unlist(lapply(GamesScores$Opponent,function(x) {
    x <- gsub(".NYJ"," Jets", x)
    x <- gsub(".NYG"," Giants", x)
    x <- gsub(".LAC"," Chargers", x)
    x <- gsub(".LAR"," Rams", x)
  }))
  GamesScores$Opponent <- OppNames
  


for (i in 1:length(GamesScores$Opponent)) {
  if (paste0(unlist(str_split(GamesScores$Opponent[i]," "))[1]
             ," ",
             unlist(str_split(GamesScores$Opponent[i]," "))[2]) == "New York" || 
      paste0(unlist(str_split(GamesScores$Opponent[i]," "))[1]
             ," ",
             unlist(str_split(GamesScores$Opponent[i]," "))[2]) == "Los Angeles") {
    next()
    
  } else if ((length(unlist(str_split(GamesScores$Opponent[i]," ")))) == 3) {
    
    GamesScores$Opponent[i] <- paste0(unlist(str_split(GamesScores$Opponent[i]," "))[1]
                            ," ",
                            unlist(str_split(GamesScores$Opponent[i]," "))[2])
  } else {
    GamesScores$Opponent[i] <- paste0(unlist(str_split(GamesScores$Opponent[i]," ")))[1]
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

## Transforming all variables 2 per Game - except those which are already or per attempt
YTDTeams <- YTDTeams %>% 
  transmute(Team,       off_pass_CMP       = round(off_pass_CMP/GP ,2)
                       ,off_pass_ATT       = round(off_pass_ATT/GP ,2)
                       ,off_pass_CMP_prc    
                       ,off_pass_AVG       = round(off_pass_AVG ,2)
                       ,off_pass_YDS_pr_G  = round(off_pass_YDS_pr_G,2)
                       ,off_pass_TD        = round(off_pass_TD/GP,2)
                       ,off_pass_INT       = round(off_pass_INT/GP,2)
                       ,off_pass_SACK      = round(off_pass_SACK/GP,2)
                       ,off_pass_RTG
                       ,off_rush_ATT       = round(off_rush_ATT/GP,2)
                       ,off_rush_AVG       = round(off_rush_AVG,2)
                       ,off_rush_YDS_pr_G  = round(off_rush_YDS_pr_G,2)
                       ,off_rush_TD        = round(off_rush_TD/GP,2)
                       ,off_rush_FUM       = round(off_rush_LST/GP,2)
                       ,off_Turnovers      = round(off_rush_LST/GP,2) + round(off_pass_INT/GP,2)
                       
                       ,def_pass_CMP       = round(def_pass_CMP/GP ,2)
                       ,def_pass_ATT       = round(def_pass_ATT/GP ,2)
                       ,def_pass_CMP_prc    
                       ,def_pass_AVG       = round(def_pass_AVG,2)
                       ,def_pass_YDS_pr_G  = round(def_pass_YDS_pr_G,2)
                       ,def_pass_TD        = round(def_pass_TD/GP,2)
                       ,def_pass_INT       = round(def_pass_INT/GP,2)
                       ,def_pass_SACK      = round(def_pass_SACK/GP,2)
                       ,def_pass_RTG
                       ,def_rush_ATT       = round(def_rush_ATT/GP,2)
                       ,def_rush_AVG       = round(def_rush_AVG,2)
                       ,def_rush_YDS_pr_G  = round(def_rush_YDS_pr_G,2)
                       ,def_rush_TD        = round(def_rush_TD/GP,2)
                       ,def_rush_FUM       = round(def_rush_LST/GP,2)
                       ,def_Turnovers      = round(def_rush_LST/GP,2) + round(def_pass_INT/GP,2))


#### MERGE GAMES & YTD STATS TABLES ####


GameswTeam <- GamesScores %>% left_join(YTDTeams, by = c("Team" = "Team"), suffix = c(".",".Team_"))

Headers <- colnames(GameswTeam)
Headers[(length(GamesScores[1,])+1):(length(colnames(GameswTeam)))] <- paste0(
  "Team_", Headers[(length(GamesScores[1,])+1):(length(colnames(GameswTeam)))])
colnames(GameswTeam) <- Headers

GameswOpp <- GameswTeam %>% left_join(YTDTeams, by = c("Opponent" ="Team"), suffix = c("_Team","_Opp"))
Headers <- colnames(GameswOpp)
Headers[(length(colnames(GameswTeam))+1):(length(colnames(GameswOpp)))] <- paste0(
  "Opp_", Headers[(length(colnames(GameswTeam))+1):(length(colnames(GameswOpp)))])
colnames(GameswOpp) <- Headers



Dataset <- GameswOpp
rm(GameswTeam,GameswOpp)

#### Take difference between teams & opponents ####
  # Decision was made to use proportions 
  #   i.e.: divide Team by Opponent
  #   e.g.: Team attended 20 passes per game, opponent 25 per game
  #         then Off_Pass_Att variable = 20/25 = 0.8
# colnames(Dataset)
Dataset <- Dataset %>% mutate( 
                    OFF_Pas_Comp    = Team_off_pass_CMP                /Opp_off_pass_CMP
                   ,OFF_pass_ATT      = Team_off_pass_ATT                /Opp_off_pass_ATT
                   ,OFF_pass_CMP_prc  = Team_off_pass_CMP_prc            /Opp_off_pass_CMP_prc
                   ,OFF_Pas_AVG       = Team_off_pass_AVG                /Opp_off_pass_AVG
                   ,OFF_pass_YDS_pr_G = Team_off_pass_YDS_pr_G           /Opp_off_pass_YDS_pr_G
                   ,OFF_pass_TD       = Team_off_pass_TD                 /Opp_off_pass_TD
                   ,OFF_pass_INT      = Team_off_pass_INT                /Opp_off_pass_INT
                   ,OFF_pass_SACK     = Team_off_pass_SACK               /Opp_off_pass_SACK
                   ,OFF_pass_RTG      = Team_off_pass_RTG                /Opp_off_pass_RTG
                   ,OFF_rush_ATT      = Team_off_rush_ATT                /Opp_off_rush_ATT
                   ,OFF_rush_AVG      = Team_off_rush_AVG                /Opp_off_rush_AVG
                   ,OFF_rush_YDS_pr_G = Team_off_rush_YDS_pr_G           /Opp_off_rush_YDS_pr_G
                   ,OFF_rush_TD       = Team_off_rush_TD                 /Opp_off_rush_TD
                   ,OFF_rush_FUM      = Team_off_rush_FUM                /Opp_off_rush_FUM
                   ,OFF_Turnovers     = Team_off_Turnovers               /Opp_off_Turnovers
                   
                   ,DEF_Pas_Comp      = Team_def_pass_CMP                /Opp_def_pass_CMP
                   ,DEF_pass_ATT      = Team_def_pass_ATT                /Opp_def_pass_ATT
                   ,DEF_pass_CMP_prc  = Team_def_pass_CMP_prc            /Opp_def_pass_CMP_prc
                   ,DEF_Pas_AVG       = Team_def_pass_AVG                /Opp_def_pass_AVG
                   ,DEF_pass_YDS_pr_G = Team_def_pass_YDS_pr_G           /Opp_def_pass_YDS_pr_G
                   ,DEF_pass_TD       = Team_def_pass_TD                 /Opp_def_pass_TD
                   ,DEF_pass_INT      = Team_def_pass_INT                /Opp_def_pass_INT
                   ,DEF_pass_SACK     = Team_def_pass_SACK               /Opp_def_pass_SACK
                   ,DEF_pass_RTG      = Team_def_pass_RTG                /Opp_def_pass_RTG
                   ,DEF_rush_ATT      = Team_def_rush_ATT                /Opp_def_rush_ATT
                   ,DEF_rush_AVG      = Team_def_rush_AVG                /Opp_def_rush_AVG
                   ,DEF_rush_YDS_pr_G = Team_def_rush_YDS_pr_G           /Opp_def_rush_YDS_pr_G
                   ,DEF_rush_TD       = Team_def_rush_TD                 /Opp_def_rush_TD
                   ,DEF_rush_FUM      = Team_def_rush_FUM                /Opp_def_rush_FUM
                   ,DEF_Turnovers     = Team_def_Turnovers               /Opp_def_Turnovers) %>% 
  dplyr::select( Team, Opponent, Team_Outcome, Winner
          ,OFF_Pas_Comp ,OFF_pass_ATT     ,OFF_pass_CMP_prc  
          ,OFF_Pas_AVG  ,OFF_pass_YDS_pr_G 
          ,OFF_pass_TD  ,OFF_pass_INT     ,OFF_pass_SACK     ,OFF_pass_RTG      
          ,OFF_rush_ATT ,OFF_rush_AVG     ,OFF_rush_YDS_pr_G ,OFF_rush_TD  ,OFF_rush_FUM, OFF_Turnovers      
          
          ,DEF_Pas_Comp ,DEF_pass_ATT      ,DEF_pass_CMP_prc  
          ,DEF_Pas_AVG  ,DEF_pass_YDS_pr_G 
          ,DEF_pass_TD  ,DEF_pass_INT      ,DEF_pass_SACK ,DEF_pass_RTG      
          ,DEF_rush_ATT ,DEF_rush_AVG      ,DEF_rush_YDS_pr_G ,DEF_rush_TD ,DEF_rush_FUM, DEF_Turnovers)



## Write source files & final Dataset to CSV & RDS objects
write.csv(Dataset, 'Data/Clean/DA2_Ass2_Cleaned_n_Merged.csv')
saveRDS(Dataset, 'Data/Clean/DA2_Ass2_Cleaned_n_Merged.rds')

write.csv(GamesScores, 'Data/Raw/DA2_Ass2_GamesScores.csv')
saveRDS(GamesScores, 'Data/Raw/DA2_Ass2_GamesScores.rds')

write.csv(YTDTeams, 'Data/Raw/DA2_Ass2_TeamStatsYTD.csv')
saveRDS(YTDTeams, 'Data/Raw/DA2_Ass2_TeamStatsYTD.rds')


