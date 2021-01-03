## CEU_DA2_Assignment2

This repository contains Bruno Helmeczy's 2nd Assignment for the Data Analysis 2 - Regressions & Coding 1 - Data Analysis & Management with R courses at the Central European Universitys' MSc in Business Analytics curriculum. 

URL to this Repository on GitHub: https://github.com/BrunoHelmeczy/CEU_DA2_Assignment2

The assignment investigated the patters of association between the outcomes of 2020 NFL season games & opposing teams' year-to-date accumulated team statistics, as of 3rd January, 2021. The repository contains the following:

### Data folder: 
**Raw data set** 
- 2020 NFL Games' outcomes were scraped from: https://www.espn.com/nfl/schedule/_/week/1, originating from the National Football League (nfl.com).  
- 2020 NFL Teams' Year-to-Date team statistics were also scraped, from https://www.espn.com/nfl/stats/team/_/view/offense/stat/passing, also originating from the National Football League (nfl.com).
- Both datasets are saved in CSV & RDS formats, the functions written to obtain them (seperately & together) are available in the Codes sub-folder.

**Cleaned & Merged data set** 
- Contains NFL game outcomes of weeks 1-16 of the 2019 & 2020 seasons, joined with Year-to-date team statistics of each games' opposing teams.
- Team statistics were 1st standardized to per-game basis. 
- To obtain unique observations per game for each team statistic, Away Teams' statistics were divided by the Home Teams' respective team statistics. 
- Throughout the analysis, Away teams' Wins were coded as "Wins", taking on the value of 1 & other outcomes as "Loss" taking the value 0. 

### Codes folder:
The codes folder includes the following:
- **Get_Game_Outcomes.R:** R function to scrape ESPN.com, to collect game outcomes used for analysis. 
  - The function takes a Year & Week Nr. as inputs, returning a dataframe of game score for the 1st specified number of weeks of the chosen season.
- **Get_Team_Stats_YTD.R:** R function to scrape ESPN.com, to collect year-to-date team statistics of every NFL team as of the time of running the function. 
  - Takes Year as input,  returning a dataframe of year-to-date accumulated statistics per team of the chosen season.
- **Clean_n_Merge_YTD_n_GamesStats.R:** R script of the data cleaning steps to obtain cleaned dataset found in the Data/Clean sub-folder.
- **F_x_Clean_n_Merge_Dataset.R:** R function to easen to data cleaning process. 
  - Takes **Get_Game_Outcomes.R's** & **Get_Team_Stats_YTD.R's** outputs as inputs, to return the cleaned datasets of the form found under the Data/Clean sub-folder.
- **NFLGameStats_ProbabilityAnalysis.R:** R script, 1st sketch of all analysis used in this project.
- **DA2_Ass2_Report.Rmd:** Finalized analysis structure of project with added text interpretations. Analysis results & reports are fully replicable with this Rmd file.

### Documents folder: 
- **.html** & **.pdf** reports - generated from **DA2_Ass2_Report.Rmd**.


