library(rvest)
library(data.table)
library(stringr)
library(dplyr)

#### PUT GAMES TOGETHER IN DATAFRAME ####

GetGameScores <- function(first_x_weeks) {

print("Creating Empty DataFrame")
AllGames <- data.frame()
  
  for (i in 1:first_x_weeks) {
  
# week1
myurl <- paste0('https://www.espn.com/nfl/schedule/_/week/',i)

# Read URL
htmlfile <- read_html(myurl) #  %>% html_nodes('.main-content') %>% html_attr('class')

# Get Headers & Columns
Headers <- htmlfile %>% html_nodes(".schedule__card") %>% html_nodes('th') %>% html_text()
Stats <- htmlfile %>% html_nodes(".schedule__card") %>% html_nodes('td') %>% html_text()

# Defines Matrices / Dataframes GamesWk1 -> GameStats
ColNames <- matrix(Headers,1,7, byrow = T)
GameStats <- as.data.frame(matrix(Stats,length(Stats)/7,7, byrow = T))

colnames(GameStats) <- ColNames

colnames(GameStats)[1:2] <- c('Away_Team', 'Home_Team')

GameStats <- GameStats[1:3]
print("Got GameStats table")

scores <- as.data.frame(matrix(unlist(lapply(GameStats[,3], function(x) {
  unlist(str_split(unlist(str_split(x,',')),' '))[c(1,2,5)]
}))
,length(unlist(lapply(GameStats[,3], function(x) {
  unlist(str_split(unlist(str_split(x,',')),' '))[c(1,2,5)]
})))/3
,3, byrow = T))

colnames(scores) <- c("Winning_Team","Points_Winner","Points_Loser")
print("Got scores table")


scores$Points_Winner <- as.numeric(scores$Points_Winner)
scores$Points_Loser <- as.numeric(scores$Points_Loser)
scores$Margin <- abs(scores$Points_Winner - scores$Points_Loser)

for (j in 1:length(GameStats[,1])) {
  
  if (is.na(scores$Margin[j])) {
    scores$Winning_Team[j] <- NA
    scores$Winner[j] <- NA
    # next()
  } else if (scores$Margin[j] == 0) {
    scores$Winning_Team[j] <- "Tie"
    scores$Winner[j] <- "Tie"
  
  } else if (length(grep(scores$Winning_Team[j],GameStats[j,2], value = T)) == 0) {
    scores$Winning_Team[j] <- GameStats[j,1]
    scores$Winner[j] <- "Away"
  } else {
    scores$Winning_Team[j] <- GameStats[j,2]
    scores$Winner[j] <- "Home"
  }
}



print(paste0("Found Winners for each game in week ",i))


df <- data.frame(GameStats[1:2],scores,Week = paste0("Wk ",i))


AllGames <- rbind(AllGames,df)
# AllGames <- AllGames %>% filter(!is.na(Margin))
}

print(AllGames)  
}







