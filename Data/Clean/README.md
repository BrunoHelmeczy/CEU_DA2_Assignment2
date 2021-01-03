# CEU_DA2_Assignment_1

This repository folder contains the cleaned NFL Games' & year-to-date NFL team statstics data, obtained from ESPN.com.

The data cleaning process was executed with the **Clean_n_Merge_YTD_n_GamesStats.R** R script, & operationalized with the **F_x_Clean_n_Merge_Dataset.R** R function, both available under this repositorys' Codes sub-folder. Its' Main points are summarized below:

**NFL Games Data:**

 - Derive Home & Away Teams from ESPN's format & rename these to Away & Home teams' respectively.
 - Convert Winner-Loser scores to Home & Away teams' scores.
 - Keep only Team, Opponent, their scores, Winners' location & calculate margin of victory/loss
 - Renomve franchise names Team/Opponent names except where more teams are present in a city (LA Rams, LA Chargers, NY Jets, NY Giants)
 
**Year-to-Date NFL Team Stats Data:**

 - Renomve franchise names Team/Opponent names except where more teams are present in a city (LA Rams, LA Chargers, NY Jets, NY Giants)
 - Transform year-to-date statistics to per game basis
 
**Merging**
  
 - Left-Join Year-to-Date statistics for Home Teams
 - Left-Join Year-to-Date statistics for Away Teams
 - Divide Away Teams' per Game Statistics with Home Teams' respective Statistics 
 
