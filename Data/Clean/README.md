# CEU_DA2_Assignment_1

This repository folder contains the cleaned Covid-19 & Country Population data, obtained from Johns Hopkins Universitys' Centre for Systems Science and Engineering (JHU CSSE) & World Banks'- World Development Indicators respectively.

The data cleaning process was executed using the **Clean_n_Merge_Covid_n_Pop_data** R script, available under this repositorys' Codes sub-folder. Its' Main points are summarized below:

**Covid Data:**

 - Remove all variables except Country_Region, Confirmed, Deaths, Recovered, * Active
 - Aggregate figures to country-level totals
 - Rename Country_Region column, to Country
 
**Population Data:**

 - Remove various geographical-region aggregations, all containing figures in iso2c column
 - Remove further grouped observations, e.g. OECD members
 - Rename variables to "Country" & "Population"
 
**Merging**
  
 - Full-Join Pre-processed Covid & Population data
 - Loop through mistakenly unmatched data to correct country names, e.g. "US" -> "United States"
 
 
**Handle Missing Values & Scale Variables**

 - Remove observations missing either of the following variables' values: Confirmed, Death, Recovered, Active, Population
 - Divide observation values by 1.000 for Confirmed, Death, Recovered, Active variables
 - Divide observation values by 1.000.000 for Population variable
