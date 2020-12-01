library(rvest)
library(data.table)
library(stringr)
library(dplyr)



GetNFLTeamStatsYTD <- function(Year) {

# 0) Creating Link combinations & FinalTable
  passrush <- c("passing","rushing","passing","rushing")
  offdef <- c("offense","offense","defense","defense")
  myurls <- paste0('https://www.espn.com/nfl/stats/team/_/view/',offdef,'/stat/', passrush,'/season/',Year,'/seasontype/2')  
  FinalTable <- data.frame()

for (i in 1:length(myurls)) {
  
  
# Getting Stats Sub-notations for each sub-table
  OffDefTag <- str_trunc(unlist(str_split(myurls[i],"/"))[9], 3, side = "right", ellipsis = '')
  PassRushTag <- str_trunc(unlist(str_split(myurls[i],"/"))[11], 4, side = "right", ellipsis = '')

# Reading Tables' Url
  t <- read_html(myurls[i])
  print(paste0("reading url Nr. ", i))
  
# Getting Data Column Names & Values Seperately
  Body <- t %>% html_nodes('.Table__TD') %>% html_text()
  Headers <- t %>% html_nodes('.Table__TH') %>% html_text()

# Extract Team Names & Remove from data values 
  Teams <- Body[1:32]
  Body <- Body[33:(length(Body))]

# Fill a matrix with data values & convert to dataframe
  Body <- as.data.frame(matrix(Body, 32, length(Headers)-1, byrow = T))

# Reattach team names 2 match column name length + use for left_join 
  Body <- cbind(Teams,Body)
  colnames(Body) <- Headers
  print(paste0("Attaching headers to datatable Nr ", i))
# Remove Comma-seperator from YDS + Convert all values' data types to numeric
  Body$YDS <- gsub(",",Body$YDS,replacement = '')

  for (j in 2:length(Body)) {
    Body[,j] <- as.numeric(Body[,j])
}
  print(paste0("Converted data types to numeric for table Nr ", i))
# Adding Sub-notations to Future Column Names
  Headers[3:(length(Headers))] <- paste0(OffDefTag,"_",PassRushTag,"_",Headers[3:(length(Headers))])
  colnames(Body) <- Headers 

# Joining Tables together
  if (length(FinalTable) == 0) {
    FinalTable <- Body
  } else {
    FinalTable <- left_join(FinalTable, Body, by = c("Team", "GP"))
}

  }
print(FinalTable)
}

