## CEU_DA2_Assignment_1

This repository contains Bruno Helmeczy's 1st Assignment for the Data Analysis 2 - Regressions & Coding 1 - Data Analysis & Management with R courses at the Central European Universitys' MSc in Business Analytics curriculum. 

URL to this Repository on GitHub: https://github.com/BrunoHelmeczy/CEU_DA2_Assignment_1

The assignment investigated the pattern of association between registered covid-19 cases and registered number of deaths due to covid-19 across countries, as of 20th October, 2020. The repository contains the following:

### Data folder: 
**Raw data set** 
- Covid-19 data is obtained from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data, originating from Johns Hopkins Universitys' Centre for Systems Science and Engineering (JHU CSSE) 2019 Novel Coronavirus Visual Dashboard.
- Listed countries' populations (as of 2019) are also included in this analysis, downloaded from the World Banks'- World Development Indicators, & available at: https://data.worldbank.org/indicator/SP.POP.TOTL. 

**Cleaned data set** 
- Contains countries' Covid-19 figures as of 20th October, 2020, including confirmed-, death-, recovered-, & active cases from 182 listed countries.
- Confirmed- & Death cases also include probable cases, if such data is reported in a country. 
- Active cases are calculated as the total confirmed cases minus deaths, minus recovered cases. 
- Covid-19 related figures were all scaled by 1.000, while countries' population figures were scaled by 1.000.000.

### Codes folder:
The codes folder includes the following:
- **Download_raw_Covid_n_Pop_data.R:** R script to collect all data used for analysis.
- **Clean_n_Merge_Covid_n_Pop_data.R:** R script to clean data set pre-analysis.
- **Analysis_Covid_n_Pop_data.R:** R script 1st sketch of all analysis used in this project.
- **DA2_Assignment1_Report.Rmd:** Finalized analysis structure of project with added text interpretation. Analysis results & reports are fully replicable with this Rmd file.

### Documents folder: 
- **.html** & **.pdf** reports - generated from **DA2_Assignment1_Report.Rmd**.


