# CLEAR MEMORY
rm(list=ls())

dfclean <- readRDS("Data/Clean/DA2_Ass2_Cleaned_n_Merged.rds")

# Import libraries
library(tidyverse)
library(lspline)
library(estimatr)
library(mfx)            # 4 Logit / Probit Models
library(margins)        # 4 Logit / Probit Models
library(pscl)           # 4 Logit / Probit Models
library(modelsummary)   # 4 Logit / Probit Models
library(huxtable)
library(ggcorrplot)
#install.packages("gmodels")
library(gmodels)

colnames(dfclean)

table(dfclean$Team_Outcome)
table(dfclean$Winner)
table(dfclean$Winner,dfclean$Team_Outcome)


#### 1) Adjustments ####

# Team variable equals Away Team
# Opponent variable equals Home Team
# Team_Outcome thus means if away team has won -> 1 = Yes ; 0 = No
dfclean$Team_Outcome <- ifelse(dfclean$Team_Outcome == "Win",1,0)
dfclean$Winner <- ifelse(dfclean$Winner == "Away", 1,0)
dfclean[c("Team_Outcome", "Winner")]
dfclean$Winner <- NULL

## We want to know who is going to win, the Home or Away team. 

# 2) Descriptives
#
# Quick check on all HISTOGRAMS
dfclean %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( dfclean )

#### HISTOGRAMS ####
#### OFFENSE ####

# 1 Passes Completed - OK - slight skewness - almost completely symmetric with log
dfclean %>% ggplot(aes(log(OFF_Pas_Comp))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 2 Passes Attended - Same as #1
dfclean %>% ggplot(aes(log(OFF_pass_ATT))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 3 Pass Completion % ( #2 / #1 ) - Same as #1
dfclean %>% ggplot(aes(log(OFF_pass_CMP_prc))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 4 Yards per passing play - Same as #1
dfclean %>% ggplot(aes(log(OFF_Pas_AVG))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 5 Passing Yards per game - visibly skewed  
dfclean %>% ggplot(aes(log(OFF_pass_YDS_pr_G))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 6 Passing Touchdowns per game - visibly skewed - definitely log
dfclean %>% ggplot(aes(log(OFF_pass_TD))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 7 Interceptions - same as #6  
dfclean %>% ggplot(aes(log(OFF_pass_INT))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 8 QB Rating - slightly skewed  - log seems to cause skewness to right
dfclean %>% ggplot(aes(log(OFF_pass_RTG))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 9 QB Sacks - very skewed - log is must
dfclean %>% ggplot(aes(log(OFF_pass_SACK))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 10 Rushing Attempts - slight skewed  
dfclean %>% ggplot(aes(log(OFF_rush_ATT))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 11 Yards per Rushing Attempt - visibly skewed log helps
dfclean %>% ggplot(aes(log(OFF_rush_AVG))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 12 Rushing Yards per game - visibly skewed - must log  
dfclean %>% ggplot(aes(log(OFF_rush_YDS_pr_G))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 13 Rushing Touchdowns - visibly skewed - must log  
dfclean %>% ggplot(aes(log(OFF_rush_TD))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 14 Fumbles - sparse data with 14 zeros - cant take logs but no distribution shape  
dfclean %>% ggplot(aes(OFF_rush_FUM)) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 15 Offensive Turnovers = Balls lost - visibly skewed - must log
dfclean %>% ggplot(aes(log(OFF_Turnovers))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

####  DEFENSIVE STATS  ####

# 1 Passes Completed - OK - slight skewness - almost completely symmetric with log
dfclean %>% ggplot(aes(log(DEF_Pas_Comp))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 2 Passes Attended - Same as #1
dfclean %>% ggplot(aes(log(DEF_pass_ATT))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 3 Pass Completion % ( #2 / #1 ) - Same as #1
dfclean %>% ggplot(aes(log(DEF_pass_CMP_prc))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 4 Yards per passing play - Same as #1
dfclean %>% ggplot(aes(log(DEF_Pas_AVG))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 5 Passing Yards per game - visibly skewed  
dfclean %>% ggplot(aes(log(DEF_pass_YDS_pr_G))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 6 Passing Touchdowns per game - visibly skewed - definitely log
dfclean %>% ggplot(aes(log(DEF_pass_TD))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 7 Interceptions - same as #6  
dfclean %>% ggplot(aes(log(DEF_pass_INT))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 8 QB Rating - slightly skewed  - log seems to cause skewness to right
dfclean %>% ggplot(aes(log(DEF_pass_RTG))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 9 QB Sacks - very skewed - log is must
dfclean %>% ggplot(aes(log(DEF_pass_SACK))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 10 Rushing Attempts - slight skewed  
dfclean %>% ggplot(aes(log(DEF_rush_ATT))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 11 Yards per Rushing Attempt - visibly skewed log helps
dfclean %>% ggplot(aes(log(DEF_rush_AVG))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 12 Rushing Yards per game - visibly skewed - must log  
dfclean %>% ggplot(aes(log(DEF_rush_YDS_pr_G))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 13 Rushing Touchdowns - visibly skewed - must log  
dfclean %>% ggplot(aes(log(DEF_rush_TD))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 14 Fumbles - sparse data with 7 zeros - cant take logs but no distribution shape  
dfclean %>% ggplot(aes(log(DEF_rush_FUM))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')

# 15 Turnovers = Balls taken - visibly skewed - must log
dfclean %>% ggplot(aes(log(DEF_Turnovers))) +
  geom_histogram( fill = 'navyblue') + 
  geom_density(color = 'red')


#### Histogram Conclusions: ####
# Taking logs results in more normal distributions in all cases 
# Except rushing fumbles: sporadic values & some underlying zeros 
# resulted in positive/negative non-finite values
# New variable was created also for this reason : Turnovers 
# Turnovers = Interceptions + Funbles
# Substantive rationale = mostly matters if you lost the ball, not if it was rushing / passing
# Possible problem: Interceptions also underlying in QBR

# Next steps: Copy data frame, drop fumbles & log-transform all variables
# Check correlations & plot conditional probabilities

####  Log_tranform like a rookie  ####

dfcleanlog <- dfclean

dfcleanlog$OFF_Pas_Comp      <- log(dfcleanlog$OFF_Pas_Comp)
dfcleanlog$OFF_pass_ATT      <- log(dfcleanlog$OFF_pass_ATT)
dfcleanlog$OFF_pass_CMP_prc  <- log(dfcleanlog$OFF_pass_CMP_prc)
dfcleanlog$OFF_Pas_AVG       <- log(dfcleanlog$OFF_Pas_AVG)
dfcleanlog$OFF_pass_YDS_pr_G <- log(dfcleanlog$OFF_pass_YDS_pr_G)
dfcleanlog$OFF_pass_TD       <- log(dfcleanlog$OFF_pass_TD)
dfcleanlog$OFF_pass_INT      <- log(dfcleanlog$OFF_pass_INT)
dfcleanlog$OFF_pass_SACK     <- log(dfcleanlog$OFF_pass_SACK)
dfcleanlog$OFF_pass_RTG      <- log(dfcleanlog$OFF_pass_RTG)
dfcleanlog$OFF_rush_ATT      <- log(dfcleanlog$OFF_rush_ATT)
dfcleanlog$OFF_rush_AVG      <- log(dfcleanlog$OFF_rush_AVG)
dfcleanlog$OFF_rush_YDS_pr_G <- log(dfcleanlog$OFF_rush_YDS_pr_G)
dfcleanlog$OFF_rush_TD       <- log(dfcleanlog$OFF_rush_TD)
dfcleanlog$OFF_rush_FUM      <- log(dfcleanlog$OFF_rush_FUM)
dfcleanlog$OFF_Turnovers     <- log(dfcleanlog$OFF_Turnovers)

dfcleanlog$DEF_Pas_Comp      <- log(dfcleanlog$DEF_Pas_Comp)
dfcleanlog$DEF_pass_ATT      <- log(dfcleanlog$DEF_pass_ATT)
dfcleanlog$DEF_pass_CMP_prc  <- log(dfcleanlog$DEF_pass_CMP_prc)
dfcleanlog$DEF_Pas_AVG       <- log(dfcleanlog$DEF_Pas_AVG)
dfcleanlog$DEF_pass_YDS_pr_G <- log(dfcleanlog$DEF_pass_YDS_pr_G)
dfcleanlog$DEF_pass_TD       <- log(dfcleanlog$DEF_pass_TD)
dfcleanlog$DEF_pass_INT      <- log(dfcleanlog$DEF_pass_INT)
dfcleanlog$DEF_pass_SACK     <- log(dfcleanlog$DEF_pass_SACK)
dfcleanlog$DEF_pass_RTG      <- log(dfcleanlog$DEF_pass_RTG)
dfcleanlog$DEF_rush_ATT      <- log(dfcleanlog$DEF_rush_ATT)
dfcleanlog$DEF_rush_AVG      <- log(dfcleanlog$DEF_rush_AVG)
dfcleanlog$DEF_rush_YDS_pr_G <- log(dfcleanlog$DEF_rush_YDS_pr_G)
dfcleanlog$DEF_rush_TD       <- log(dfcleanlog$DEF_rush_TD)
dfcleanlog$DEF_rush_FUM      <- log(dfcleanlog$DEF_rush_FUM)
dfcleanlog$DEF_Turnovers     <- log(dfcleanlog$DEF_Turnovers)


#### log-transform all features / variables ####
dfcleanlog <- cbind(dfclean[1:3],lapply(colnames(dfclean)[4:length(colnames(dfclean))], function(x) {
  tl <- list()
  tl[[x]] <- log(dfclean[c(x)]) 
  return(tl)
}))





#### 3) Check Correlations ####
# The feature space is 28 variables, with expected observation count of 240
# 2 Obvious candidates to drop some variables are via Passing QBR:
#     It incorporates 1) Passes Completed & 2) Attended, 3) Passing Yards/Attempt
#                     4) Touchdowns & 5) Interceptions
# Thus collinearity is expected among QBR & these vars -> Opp 2 drop 10 vars
#
### 3.1) 1st w original data 
numeric_df <- keep( dfclean , is.numeric )
cT <- round(cor(numeric_df , use = "complete.obs"),3)

data.frame(abs(cT))[c("OFF_pass_RTG")] %>% filter(OFF_pass_RTG != 1, na.rm == T) %>% 
  arrange(desc(OFF_pass_RTG))

ggcorrplot(cT,method = "square", type = "upper"
           , colors = c("red","white","red")
           ,lab = T, tl.cex = 6, lab_size = 1)

# Check for highly correlated values:
sum( abs(cT) >= 0.8 & abs(cT) != 1 , na.rm = T) / 2
sum( abs(cT) >= 0.65 & abs(cT) != 1 , na.rm = T) / 2

# Find the correlations which are higher than 0.8
id_cr <- which( abs(cT) >= 0.8 & abs(cT) != 1 )
pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr %>% arrange(desc(abs(corr_val)))

# Find the correlations which are higher than 0.65
id_cr <- which( abs(cT) >= 0.7 & abs(cT) != 1 )
pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr %>% arrange(desc(abs(corr_val)))



### 3.2) 2nd w log-transformed data 
numeric_df2 <- keep( dfcleanlog , is.numeric )
cT2 <- round(cor(numeric_df2 , use = "complete.obs"),3)


ggcorrplot(cT2,method = "square", type = "upper"
           , colors = c("red","white","red")
           ,lab = T, tl.cex = 5, lab_size = 1)




# Check for highly correlated values:
sum( abs(cT) >= 0.7 & abs(cT) != 1 , na.rm = T) / 2
# Find the correlations which are higher than 0.8
id_cr <- which( abs(cT) >= 0.7 & abs(cT) != 1 )
pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr %>% arrange(desc(abs(corr_val)))

#### 4) Functional form checks ####

CondWinProbability <- function(df, x_var) {
  df %>% ggplot( aes(x=x_var, y=Team_Outcome)) +
    geom_point(color="red", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
    geom_smooth(method="loess", color="blue") + 
    scale_x_continuous(expand = c(0.01,0.01), limits = c(min(x_var),max(x_var))
                       , breaks = seq(round(min(x_var),2),round(max(x_var),2)
                                      ,round((round(max(x_var),2)-round(min(x_var),2))/20,2)))+
    scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1)
                       , breaks = seq(0,1,0.1)) +
    theme(axis.text.x = element_text(angle = 45, size = 8)) +
    labs(x = "x_var" ,y = "Probability of Team Winning ")  
}
dfclean$Team_Outcome <- as.integer(dfclean$Team_Outcome)
dfcleanlog$Team_Outcome <- as.integer(dfcleanlog$Team_Outcome)

## QB Ratings 
CondWinProbability(dfclean,dfclean$OFF_pass_RTG)
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_RTG)

CondWinProbability(dfclean,dfclean$DEF_pass_RTG)
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_RTG)

#### OFFENSE ####
# Passes Attempted -> Seems Mean-independent - LEAVE
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_ATT)
CondWinProbability(dfclean,dfclean$OFF_pass_ATT)

# Passes Completion % -> Cubic form - Already in RTG
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_CMP_prc)
CondWinProbability(dfclean,dfclean$OFF_pass_CMP_prc)

# Passing yards per game -> Cubic Form - Try
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_YDS_pr_G)
CondWinProbability(dfclean,dfclean$OFF_pass_YDS_pr_G)

# Passing Touchdowns per game -> High Correl w RTG
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_TD) # Cubic
CondWinProbability(dfclean,dfclean$OFF_pass_TD) # Break @ ca. 1.4

# Passing Interceptions per game -> High Correl w RTG 
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_INT) # Linear
CondWinProbability(dfclean,dfclean$OFF_pass_INT) 
  # Linear -> Maybe Quadratic

# Sacks per game -> 
CondWinProbability(dfcleanlog,dfcleanlog$OFF_pass_SACK) 
  # Mean-Indy <-0.25, Quadratic after
CondWinProbability(dfclean,dfclean$OFF_pass_SACK) # Quadratic

# Passing yards per attempt -> 
CondWinProbability(dfcleanlog,dfcleanlog$OFF_Pas_AVG) # Linear
CondWinProbability(dfclean,dfclean$OFF_Pas_AVG)

# Passes Completed per game -> 
CondWinProbability(dfcleanlog,dfcleanlog$OFF_Pas_Comp) # PLS: -0.1 / 0.05
CondWinProbability(dfclean,dfclean$OFF_Pas_Comp) # PLS: 0.9 / 1.1 / 1.35

# Rushing Attempts per game -> PLS
CondWinProbability(dfcleanlog,dfcleanlog$OFF_rush_ATT) 
  # PLS -0.08 / 0.02  
CondWinProbability(dfclean,dfclean$OFF_rush_ATT) 
  # PLS 0.92 / 1

# Rushing yards per attempt -> Cubic but mean-indy
CondWinProbability(dfcleanlog,dfcleanlog$OFF_rush_AVG) 
CondWinProbability(dfclean,dfclean$OFF_rush_AVG) 

# Rushing yards per game -> Cubic but mean-indy
CondWinProbability(dfcleanlog,dfcleanlog$OFF_rush_YDS_pr_G) # Cubic
CondWinProbability(dfclean,dfclean$OFF_rush_YDS_pr_G) # Quadratic

# Rushing touchdowns per game -> Cubic
CondWinProbability(dfcleanlog,dfcleanlog$OFF_rush_TD) # Cubic
CondWinProbability(dfclean,dfclean$OFF_rush_TD) # Linear

# Rushing Fumbles per game -> Mean Indy
CondWinProbability(dfcleanlog,dfcleanlog$OFF_rush_FUM) # Mean-Indy
CondWinProbability(dfclean,dfclean$OFF_rush_FUM) # Mean-Indy - Decrease after 3

# Offensive Turnovers -> PLS: 
CondWinProbability(dfcleanlog,dfcleanlog$OFF_Turnovers) # -0.5 / 1 (Mean-Indy in Middle)
CondWinProbability(dfclean,dfclean$OFF_Turnovers) # Mean-Indy (0.8 - 4)


#### DEFENSE ####
# Passes Attempted -> 
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_ATT) # Lin+ <0.03
CondWinProbability(dfclean,dfclean$DEF_pass_ATT) # Lin+ <1.05

# Passes Completion % -> Cubic form
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_CMP_prc)
CondWinProbability(dfclean,dfclean$DEF_pass_CMP_prc)

# Passing yards per game -> Cubic Form
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_YDS_pr_G)
CondWinProbability(dfclean,dfclean$DEF_pass_YDS_pr_G)

# Passing Touchdowns per game -> 
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_TD) # Quadratic
CondWinProbability(dfclean,dfclean$DEF_pass_TD) # Lin- <1.1 & Mean-Indy

# Passing Interceptions per game -> 
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_INT) # Cubic
CondWinProbability(dfclean,dfclean$DEF_pass_INT) # Quadratic

# Sacks per game -> 
CondWinProbability(dfcleanlog,dfcleanlog$DEF_pass_SACK) # Cubic
CondWinProbability(dfclean,dfclean$DEF_pass_SACK) # PLS break @ 1.6

# Passing yards per attempt -> CUBIC
CondWinProbability(dfcleanlog,dfcleanlog$DEF_Pas_AVG) # Linear
CondWinProbability(dfclean,dfclean$DEF_Pas_AVG)

# Passes Completed per game -> PLS
CondWinProbability(dfcleanlog,dfcleanlog$DEF_Pas_Comp) # PLS: -0.15 / 0.07
CondWinProbability(dfclean,dfclean$DEF_Pas_Comp) # PLS: 0.9 / 1.1 / 1.35

# Rushing Attempts per game -> PLS
CondWinProbability(dfcleanlog,dfcleanlog$DEF_rush_ATT) # PLS: 0  
CondWinProbability(dfclean,dfclean$DEF_rush_ATT) # PLS: 1

# Rushing yards per attempt -> Cubic
CondWinProbability(dfcleanlog,dfcleanlog$DEF_rush_AVG) 
CondWinProbability(dfclean,dfclean$DEF_rush_AVG) 

# Rushing yards per game -> Cubic but mean-indy
CondWinProbability(dfcleanlog,dfcleanlog$DEF_rush_YDS_pr_G) # Cubic
CondWinProbability(dfclean,dfclean$DEF_rush_YDS_pr_G) # Quadratic

# Rushing touchdowns per game -> Cubic
CondWinProbability(dfcleanlog,dfcleanlog$DEF_rush_TD) # Cubic
CondWinProbability(dfclean,dfclean$DEF_rush_TD) # Linear

# Rushing Fumbles per game -> Mean Indy
CondWinProbability(dfcleanlog,dfcleanlog$DEF_rush_FUM) # Mean-Indy
CondWinProbability(dfclean,dfclean$DEF_rush_FUM) # Mean-Indy - Decrease after 3

# Defensive Turnovers -> PLS: 
CondWinProbability(dfcleanlog,dfcleanlog$DEF_Turnovers) # W-shaped
CondWinProbability(dfclean,dfclean$DEF_Turnovers) # Quadratic


#### 1st LPMs ####
# 1st model: Team Outcome only function of which Teams' QB plays better
lpm1 <- lm( Team_Outcome ~ OFF_pass_RTG ,data = dfcleanlog)
summary(lpm1,vcov = sandwich)
dfcleanlog$pred1 <- predict(lpm1)

# 1st Confusion Table: Predicted Probabilities rounded to integers
table(round(dfcleanlog$pred1,0),dfcleanlog$Team_Outcome)
sum(round(dfcleanlog$pred1,0) == dfcleanlog$Team_Outcome)/length(dfcleanlog$Team_Outcome)

# 2nd model: Function of Offensive QB performance & defense vs opponent QB
lpm2 <- lm( Team_Outcome ~ OFF_pass_RTG + DEF_pass_RTG ,data = dfcleanlog)
summary(lpm2, vcov = sandwich )
dfcleanlog$pred2 <- predict(lpm2)
table(round(dfcleanlog$pred2,0),dfcleanlog$Team_Outcome)
sum(round(dfcleanlog$pred2,0) == dfcleanlog$Team_Outcome)/length(dfcleanlog$Team_Outcome)
  
#### 3rd #### 
lpm3 <- lm( Team_Outcome ~ OFF_pass_RTG + DEF_pass_RTG + OFF_rush_TD, data = dfcleanlog)
summary(lpm3, vcov = sandwich )
dfcleanlog$pred3 <- predict(lpm3)
table(round(dfcleanlog$pred3,0),dfcleanlog$Team_Outcome)
sum(round(dfcleanlog$pred3,0) == dfcleanlog$Team_Outcome)/length(dfcleanlog$Team_Outcome)

#### 4th ####
lpm4 <- lm( Team_Outcome ~ OFF_pass_RTG + DEF_pass_RTG + OFF_rush_AVG + 
              lspline(OFF_Turnovers, c(-0.7, 0.8)) + DEF_Turnovers, data = dfcleanlog)
summary(lpm4, vcov = sandwich )
dfcleanlog$pred4 <- predict(lpm4)
table(round(dfcleanlog$pred4,0),dfcleanlog$Team_Outcome)
sum(round(dfcleanlog$pred4,0) == dfcleanlog$Team_Outcome)/length(dfcleanlog$Team_Outcome)


#### 5th ####
lpm5 <- lm( Team_Outcome ~ OFF_pass_RTG + DEF_pass_RTG + OFF_rush_AVG + OFF_rush_TD +
            lspline(OFF_Turnovers, c(-0.7, 0.8)) + DEF_Turnovers, data = dfcleanlog)
summary(lpm5, vcov = sandwich )
dfcleanlog$pred5 <- predict(lpm5)
table(round(dfcleanlog$pred5,0),dfcleanlog$Team_Outcome)

high_corr

# Compare models w jTools & Huxtable

exptbl <- export_summs(lpm1, lpm2,
                       model.names = c("Offensive QB Rating",
                                       "Offensive & Defensive QB Rating")
                       ,to.file = "html"
                       ,file.name = "Base2Models.html")


as_hux(exptbl)

#### Use non-log-transformed df ####

#### 1) log(Offense QB) ####
lpmclean <- lm(Team_Outcome ~ log(OFF_pass_RTG), data = dfclean)
summary(lpmclean, vcov = sandwich )
dfclean$pred1 <- predict(lpmclean)
table(round(dfclean$pred1,0),dfclean$Team_Outcome)
sum(round(dfclean$pred1,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)

#### 2) log(Offense QB) & log(Defense QB) ####
lpmclean2 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG)
                 , data = dfclean)
summary(lpmclean2, vcov = sandwich )
dfclean$pred2 <- predict(lpmclean2)
table(round(dfclean$pred2,0),dfclean$Team_Outcome)
sum(round(dfclean$pred2,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)

#### 3) #2 + PassYards_GP   ####
dfclean$OFF_pass_YDS_pr_G_ln_sq <- (log(dfclean$OFF_pass_YDS_pr_G))^2
dfclean$OFF_pass_YDS_pr_G_ln_cb <- (log(dfclean$OFF_pass_YDS_pr_G))^3

lpmclean3 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                  log(OFF_pass_YDS_pr_G) + OFF_pass_YDS_pr_G_ln_sq +
                  OFF_pass_YDS_pr_G_ln_cb
                , data = dfclean)

summary(lpmclean3, vcov = sandwich )
dfclean$pred3 <- predict(lpmclean3)
table(round(dfclean$pred3,0),dfclean$Team_Outcome)
sum(round(dfclean$pred3,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)
# Conclusion: No improvement - 66% correlation betw log(QB Rating) & log(PassYards_GP)
#### 4) logs(Offense & Defense QB) + Offense Sacks ####
dfclean$OFF_pass_SACK_ln_sq <- NULL
dfclean$OFF_pass_SACK_ln_cb <- NULL


lpmclean4 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                  OFF_pass_SACK_dummy  
                , data = dfclean)

summary(lpmclean4, vcov = sandwich )
dfclean$pred4 <- predict(lpmclean4)
table(round(dfclean$pred4,0),dfclean$Team_Outcome)
sum(round(dfclean$pred4,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)

# Sacks improve the model in given range -> Dummy for when -0.2 <= log(offense_sacks) < 1 
dfclean$OFF_pass_SACK_dummy <- log(dfclean$OFF_pass_SACK)*ifelse(log(dfclean$OFF_pass_SACK) >= -0.2 & 
                                        log(dfclean$OFF_pass_SACK) <= 1,1,0)



#### 5) #4 + log(OFF_rush_AVG)  ####

lpmclean5 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                  OFF_pass_SACK_dummy +
                  log(OFF_rush_AVG)

                , data = dfclean)

summary(lpmclean5, vcov = sandwich )
dfclean$pred5 <- predict(lpmclean5)
table(round(dfclean$pred5,0),dfclean$Team_Outcome)
sum(round(dfclean$pred5,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)

#### 6) #5 + poly(log(Offensive_Rushing_TDs),2)  #### 
dfclean$OFF_rush_TD_ln_sq <- (log(dfclean$OFF_rush_TD))^2
dfclean$OFF_rush_YDS_pr_G_ln_cb <- NULL

lpmclean6 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                  OFF_pass_SACK_dummy +
                  log(OFF_rush_AVG) + log(OFF_rush_TD) +
                  OFF_rush_TD_ln_sq
                , data = dfclean)

summary(lpmclean6, vcov = sandwich )
dfclean$pred6 <- predict(lpmclean6)
table(round(dfclean$pred6,0),dfclean$Team_Outcome)
sum(round(dfclean$pred6,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)

#### 7) #6 + poly(log(Off_turnovers),3)####
dfclean$OFF_Turnovers_ln_sq <- (log(dfclean$OFF_Turnovers))^2
dfclean$OFF_Turnovers_ln_cb <- (log(dfclean$OFF_Turnovers))^3

lpmclean7 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                  OFF_pass_SACK_dummy +
                  log(OFF_rush_AVG) + 
                  #log(OFF_rush_TD) +  
                  OFF_rush_TD_ln_sq +
                  log(OFF_Turnovers) + 
                  #OFF_Turnovers_ln_sq + 
                  OFF_Turnovers_ln_cb
                , data = dfclean)

summary(lpmclean7, vcov = sandwich )
dfclean$pred7 <- predict(lpmclean7)
table(round(dfclean$pred7,0),dfclean$Team_Outcome)
sum(round(dfclean$pred7,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)

#### 8) #7 +  ####

dfclean$DEF_pass_SACK_ln_sq <- NULL
dfclean$DEF_pass_SACK_ln_cb <- NULL

lpmclean8 <- lm(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                  OFF_pass_SACK_dummy +
                  log(OFF_rush_AVG) + 
                  #log(OFF_rush_TD) +  
                  OFF_rush_TD_ln_sq +
                  log(OFF_Turnovers) + 
#                  OFF_Turnovers_ln_sq + 
                  OFF_Turnovers_ln_cb 
                , data = dfclean)

summary(lpmclean8, vcov = sandwich )
dfclean$pred8 <- predict(lpmclean8)
table(round(dfclean$pred8,0),dfclean$Team_Outcome)
sum(round(dfclean$pred8,0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome)


#### Summarize Models: #### 
# Compare models w jTools & Huxtable

exptbl <- export_summs(lpmclean, lpmclean2,lpmclean3,lpmclean4
                       ,lpmclean5,lpmclean6,lpmclean7
                       ,model.names = c("Off QBR","#1 +Def QBR","#2+PassYards"
                                        ,"#2+Off Sacks","#4+Off Rush AVG"
                                        ,"#5+Off RushTDs","#6+Off Turnovers")
                       ,to.file = "html"
                       ,file.name = "7Models.html")


as_hux(exptbl)

# Model Prediction Summary
MinMax <- sum_stat(dfclean,
         c("pred1","pred2","pred3","pred4"
           ,"pred5","pred6","pred7"),
         c("mean","median","min","max","sd"))


CrossTable(dfclean$Team_Outcome, round(dfclean$pred7,0))
table(dfclean$Team_Outcome, round(dfclean$pred1,0))


PredAcc <- as.data.frame(lapply(c("pred1","pred2","pred3","pred4"
              ,"pred5","pred6","pred7"),function(x) {
        tl <- list()
        tl[[x]] <- round(sum(round(dfclean[x],0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome),3)*100
        return(tl)
              }))

PredAcc$statistics <- "Prediction Acc%"

Briers <- as.data.frame(lapply(c("pred1","pred2","pred3","pred4"
                       ,"pred5","pred6","pred7"),function(x) {
                         tl <- list()
                         tl[[x]] <- round(sum(  (dfclean[c(x)]-dfclean[c("Team_Outcome")])^2)/count(dfclean[c("Team_Outcome")]),3)
                         names(tl[[x]]) <- x
                         return(tl)
                       }))
Briers$statistics <- "Brier-Score"

SumStatTbl <- rbind(MinMax,PredAcc,Briers)

# Show the predicted probabilities' distribution (ggplot)

dfclean %>% ggplot(aes(alpha = 0.2)) +
  geom_histogram(aes(x = pred1),fill = 'navyblue', color = 'grey90') +
#  geom_histogram(aes(x = pred2),fill = 'blue', color = 'grey90') +
#  geom_histogram(aes(x = pred3),fill = 'lightblue', color = 'grey90') +
#  geom_histogram(aes(x = pred4),fill = 'green', color = 'grey90') +
 # geom_histogram(aes(x = pred5),fill = 'orange', color = 'grey90') +
#  geom_histogram(aes(x = pred6),fill = 'red', color = 'grey90') +
  geom_histogram(aes(x = pred7),fill = 'lightblue', color = 'grey90') +
  theme_minimal()

#### Probits & Logits ####
  
  model_formula <- formula(Team_Outcome ~ log(OFF_pass_RTG) + log(DEF_pass_RTG) + 
                             OFF_pass_SACK_dummy +
                             log(OFF_rush_AVG) + 
                             #log(OFF_rush_TD) +  
                             OFF_rush_TD_ln_sq +
                             log(OFF_Turnovers) + 
                             #                  OFF_Turnovers_ln_sq + 
                             OFF_Turnovers_ln_cb )


lpmfull <- lm (model_formula, data = dfclean)
summary(lpmfull, vcov = sandwich)

#### LOGIT ####
#   alternatively: family='binomial' automatically gives you logit, but not probit...
logit <- glm( model_formula , data=dfclean, family=binomial(link="logit") )
summary(logit)
glance(logit)

# predicted probabilities 
dfclean$pred7_logit <- predict.glm(logit, type="response")
summary(dfclean$pred7_logit)
table(round(dfclean$pred7_logit,0),dfclean$Team_Outcome)
table(round(dfclean$pred7,0),dfclean$Team_Outcome)



# Calculate logit marginal differences
logit_marg <- logitmfx( model_formula, data=dfclean, atmean=FALSE, robust = T)
print(logit_marg)


#### PROBIT ####
#   alternatively: family='binomial' automatically gives you logit, but not probit...
probit <- glm( model_formula , data=dfclean, family=binomial(link="probit") )
summary(probit)
glance(probit)

# predicted probabilities 
dfclean$pred7_probit <- predict.glm(probit, type="response")
summary(dfclean$pred7_probit)
table(round(dfclean$pred7_probit,0),dfclean$Team_Outcome)
table(round(dfclean$pred7,0),dfclean$Team_Outcome)

# Calculate logit marginal differences
probit_marg <- probitmfx( model_formula, data=dfclean, atmean=FALSE, robust = T)
print(probit_marg)


#### LPM / Logit / Probit summaries ####
cm <- c('(Intercept)' = 'Constant')
pmodels <- list(lpmfull, logit, logit_marg, probit, probit_marg)
pmodels2 <- list(lpmfull, logit_marg,  probit_marg)
msummary( pmodels2 ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|F',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
#          coef_omit = 'as.factor(country)*',
          output = paste0("prob_models_coeff.html")
)



dfclean %>% ggplot(aes(x = pred7)) +
  geom_point(aes(y = pred7_logit, color = "Logit"), size = 0.5) +
  geom_point(aes(y = pred7_probit, color = "Probit"), size = 0.5) +
  geom_line(aes(y = pred7, color = "45Degree Line")) 


Win <- sum_stat(subset(dfclean, Team_Outcome ==1),
               c("pred1","pred7","pred7_logit","pred7_probit"),
               c("mean","median","min","max","sd"))

Loss <- sum_stat(subset(dfclean, Team_Outcome ==0),
                c("pred1","pred7","pred7_logit","pred7_probit"),
                c("mean","median","min","max","sd"))


ProbModsTbl <- sum_stat(dfclean,
         c("pred1","pred7","pred7_logit","pred7_probit"),
         c("mean","median","min","max","sd"))


ProbModsAcc <- as.data.frame(lapply(c("pred1","pred7","pred7_logit","pred7_probit"),function(x) {
  tl <- list()
  tl[[x]] <- round(sum(round(dfclean[x],0) == dfclean$Team_Outcome)/length(dfclean$Team_Outcome),3)*100
  return(tl)
  }))
ProbModsAcc$statistics <- "Prediction Acc%"

ProbModsBriers <- as.data.frame(lapply(c("pred1","pred7","pred7_logit","pred7_probit"),function(x) {
  tl <- list()
  tl[[x]] <- round(sum(  (dfclean[c(x)]-dfclean[c("Team_Outcome")])^2)/count(dfclean[c("Team_Outcome")]),3)
  names(tl[[x]]) <- x
  return(tl)
  }))
ProbModsBriers$statistics <- "Brier-Score"

# Biased prediction? Calculate bias!
Bias <- as.data.frame(lapply(c("pred1","pred7","pred7_logit","pred7_probit"), function(x) {
  tl <- list()
  Pred <- sum(dfclean[c(x)]) / count(dfclean[c(x)])
  Act <- sum(dfclean[c("Team_Outcome")]) / count(dfclean[c("Team_Outcome")])
  
  tl[[x]] <- round(Pred - Act,3)
  names(tl[[x]]) <- x
  return(tl)
}))
Bias$statistics <- "Bias"

ProbModFinTbl <- rbind(ProbModsTbl,ProbModsAcc,ProbModsBriers,Bias)


####  CALIBRATION CURVES  ####
GetCalibrationCurve <- function(df, outcome, chosenmodel, Nr_groups) {
  actual_vs_predicted <- df[c(outcome, chosenmodel)]
  num_groups <- Nr_groups
  actual_vs_predicted$predicted <- df[c(chosenmodel)]
  actual_vs_predicted$actual <- df[c(outcome)]
  
  calibration_d <- actual_vs_predicted %>%
    mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
    group_by(predicted_score_group) %>%
    dplyr::summarise(mean_actual = (sum(actual)/count(actual)), 
                     mean_predicted = (sum(predicted)/count(predicted)), 
                     num_obs = n())
  
  ggplot( calibration_d,aes(x = mean_actual$n, y = mean_predicted$n)) +
    geom_point( color='red', size=1.5, alpha=0.8) +
    geom_line(  color='red', size=1  , alpha=0.8) +
    geom_abline( intercept = 0, slope = 1, color='blue') +
    labs( x = "Actual event probability", y = "Predicted event probability") +
    scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
    scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1))  
}


GetCalibrationCurve(dfclean,"Team_Outcome","pred1",10)
GetCalibrationCurve(dfclean,"Team_Outcome","pred7",10)
GetCalibrationCurve(dfclean,"Team_Outcome","pred7_logit",10)
GetCalibrationCurve(dfclean,"Team_Outcome","pred7_probit",10)

#### BACKTESTING WITH 2019 DATA ####

source('Codes/Get_Team_Stats_YTD.R')
source('Codes/Get_Game_Outcomes.R')
source('Codes/F_x_Clean_n_Merge_Dataset.R')

dftesting <- CleanedNFL_df(GetGameScores(,2019),GetNFLTeamStatsYTD(2019))
dftesting$Team_Outcome <- ifelse(dftesting$Team_Outcome == "Win",1,0)
dftesting$OFF_pass_SACK_dummy <- log(dftesting$OFF_pass_SACK)*ifelse(log(dftesting$OFF_pass_SACK) >= -0.2 & 
                                                                   log(dftesting$OFF_pass_SACK) <= 1,1,0)
dftesting$OFF_rush_TD_ln_sq <- log(dftesting$OFF_rush_TD)^2
dftesting$OFF_Turnovers_ln_cb <- log(dftesting$OFF_Turnovers)^3

data.frame(log(dftesting$OFF_Turnovers),poly(log(dftesting$OFF_Turnovers),3, raw = T,simple = T))[,1:2]



  #### Base LPM ####
dftesting$pred1 <- lpmclean$coefficients[1] + lpmclean$coefficients[2]*log(dftesting$OFF_pass_RTG)


table(dftesting$Team_Outcome,round(dftesting$pred1,0))
round((sum(dftesting$Team_Outcome == round(dftesting$pred1,0))/length(dftesting$Team_Outcome))*100,2)

  #### FULL LPM ####
dftesting$pred7 <- lpmclean7$coefficients[1] +
lpmclean7$coefficients[2]*log(dftesting$OFF_pass_RTG) + 
lpmclean7$coefficients[3]*log(dftesting$DEF_pass_RTG) +
lpmclean7$coefficients[4]*dftesting$OFF_pass_SACK_dummy + 
lpmclean7$coefficients[5]*log(dftesting$OFF_rush_AVG) +
lpmclean7$coefficients[6]*(log(dftesting$OFF_rush_TD)^2) + 
lpmclean7$coefficients[7]*log(dftesting$OFF_Turnovers) + 
lpmclean7$coefficients[8]*(log(dftesting$OFF_Turnovers)^3)




table(dftesting$Team_Outcome,round(dftesting$pred7,0))
round((sum(dftesting$Team_Outcome == round(dftesting$pred7,0))/length(dftesting$Team_Outcome))*100,2)

  #### Logit & Probit ####
  model_formula

  #### Full Model - Logit ####
dftesting$pred7_logit <- predict.glm(logit, type="response", newdata = dftesting)

table(dftesting$Team_Outcome,round(dftesting$pred7_logit,0))
round((sum(dftesting$Team_Outcome == round(dftesting$pred7_logit,0))/length(dftesting$Team_Outcome))*100,2)


  #### Full Model - Probit ####
dftesting$pred7_probit <- predict.glm(probit, type="response", newdata = dftesting)

table(dftesting$Team_Outcome,round(dftesting$pred7_probit,0))
round((sum(dftesting$Team_Outcome == round(dftesting$pred7_probit,0))/length(dftesting$Team_Outcome))*100,2)



colnames(dfclean)
