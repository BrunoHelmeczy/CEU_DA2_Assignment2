# CEU_DA2_Assignment_1 - Code Files

This repository folder contains 3 R-Scripts, together with the final Assignment report using the merged Covid-19 & Country Population data available under this repositorys' Data/Clean sub-folder, in RMarkdown format. Below is a bulletpoint outline on each files' contribution to the final assigment reports, available within the Documents sub-folder:

**1) Download_raw_Covid_n_Pop_data.R**
 - Download Covid-19 data from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data & write into csv file (available under Data/Raw sub-folder)
 - Download Countries' Population figures as of 2019, from https://data.worldbank.org/indicator/SP.POP.TOTL. using the WDI R package & write into csv file (available under Data/Raw sub-folder)

**2) Clean_n_Merge_Covid_n_Pop_data.R**
 - Remove Covid-19 variables except Country_Region, Confirmed, Deaths, Recovered,  Active
 - Aggregate Covid-19 figures to country-level totals
 - Retain only Country-level Population Data
 - Full-Join Covid & Population data & correct mistakenly unmatching values
 - Remove observations with Missing Values & Scale Variables
 - Write into csv file (available under Data/Clean sub-folder)
 
**3) Analysis_Covid_n_Pop_data.R**
  - Summarize Population, Cases, & Deaths statistics & visualize histograms 
  - Investigate Cases' & Deaths' associative pattern with scatter-plots under combinations of logarithmic tranformations
  - Conclude Log-Log assocation is best candidate & remove countries without deaths
  - Model & Visualize association pattern in Log-Log form, using Linear-, Quadratic-, Piecewise Linear Spline-, & Population Weighted Linear function forms
  - Test whether Population Weighted Linear models' Slope parameter is Zero
  - Analyze countries with 5 largest positive / negative residuals relative to chosen Population Weighted Linear model

**4) DA2_Assignment1_Report.Rmd**
 - Add Textual interpretation based on Analysis R-script
 - Format tables & visuals
