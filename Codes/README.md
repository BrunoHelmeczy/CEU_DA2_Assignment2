# CEU_DA2_Assignment2 - Code Files

This repository folder contains 3 R-Scripts, together with the final Assignment report using the merged 2019 & 2020 NFL data available under this repositorys' Data/Clean sub-folder, in RMarkdown format. Below is a bulletpoint outline on each files' contribution to the final assigment reports, available within the Documents sub-folder:

**1) Get_Game_Outcomes.R**
 - Loop through every weeks' url page & read it as html
 - Obtain Game Score html tables
 - Convert obtained values to numeric formats
 - Aggregate games data week-by-week
 - Return final Aggregated data as dataframe

**2) Get_Team_Stats_YTD.R**
 - Loop through the NFLs' Offensive & Defensive Passing & Rushing stats pages' url pages & read as html
 - Obtain Year-to-date html tables
 - Convert obtained values to numeric formats
 - Aggregate year-to-date data
 - Return Aggregated data as dataframe

 - Remove Covid-19 variables except Country_Region, Confirmed, Deaths, Recovered,  Active
 - Aggregate Covid-19 figures to country-level totals
 - Retain only Country-level Population Data
 - Full-Join Covid & Population data & correct mistakenly unmatching values
 - Remove observations with Missing Values & Scale Variables
 - Write into csv file (available under Data/Clean sub-folder)

**3) Clean_n_Merge_YTD_n_GamesStats.R**
  - Rename Home & Away teams to "Team" & "Opponent" Respectively
  - Calculate Outcome variable & convert Winner-Loser format from ESPN to Team-Opponent scores
  - Retain only Team names, Win-Loss outcomes, Teams' Points scored & calculate Margin of win-loss
  - Keep only city names except where 2 teams are in 1 city (Los Angeles & New York)
  - Repeat previous step for year-to-date stats data
  - Standardize all year-to-date statistics to per-game basis
  - Left-Join Year-to-date stats to nfl games home teams
  - Left-Join Year-to-date stats to nfl games away teams
  - Divide Away Teams' all statistics by Home Teams' respective statistics

**4) F_x_Clean_n_Merge_Dataset.R**
  - Transform cleaning process in **3) Clean_n_Merge_YTD_n_GamesStats.R** into function taking **1) Get_Game_Outcomes.R** & **2) Get_Team_Stats_YTD.R** functions' outputs as inputs.

**5) NFLGameStats_ProbabilityAnalysis.R**
  - Summarize per game Away/Home Team NFL statistics & visualize histograms & log-transformations
  - Visualize Variable correlations with Correlogram
  - Approximate association patterns with Conditional Win Probabilities, using LOESS curves on scatter-plots & visualize most appropriate transformations from linear, logarithmic, polynomial, piecewise linear spline tranformations.
  - Add Sacks Suffered, Rushing Yards per Attempt, Offensive Turnovers & Sacks enforced as explanatory variables, for the full linear probability model.
  - Interpret & Compare full Linear Probability Model with Logit & Probit Formulations, using Bias, Brier-Score, Prediction accuracy & Calibration Curves.
  - As robustness check, re-estimate chosen Probit model using the 2019 data  
  - Conclude Offensive & Defensive Passer Ratings to be only robust variables from full Probit model 
 - Test Probit Model prediction accuracy on 2019 data for external validity

**6) DA2_Ass2_Report.Rmd**
 - Add Textual interpretation based on Probability Analysis R script
 - Standardize tables & visuals
