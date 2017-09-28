#Primary Research Question
#What model best describes the first decade of internet usage (1990-1999) in the United States? 
#Which model is a better long-term fit?


library(SDSFoundations)
View(WorldBankData)
world<-WorldBankData

# Subset data for just the United States and name the new data frame "us"
us <- world[world$Country.Code == "USA",]

# Select the years from 1990 and name the new data frame "us_select"
us_select <- us[us$year >= 1990, ]

# Create a new variable in our datset called internet.mil to make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1000000

# Create a new variable in our dataset called time that represents "years since 1990"
us_select$time <- us_select$year - 1990

# Select the first 10 years (from 1990 to 1999) and name the new data frame "us_select_10"
us_select_10 <- us_select[us_select$time < 10,]

# Use a function to fit an exponential and logistic model for 1990-1999
expFit(us_select_10$time, us_select_10$internet.mil)
logisticFit(us_select_10$time, us_select_10$internet.mil)

# Based on the prior model parameters, predict the number of internet users in 2006
e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)
l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)

#Secondary Research Question
#Denmark is a high-income country in Europe of about 5.5 million people. 
#What is the best-fitting model for growth of internet usage in Denmark since 1990? 

#Create dataframe with only with Denmark info.
denmarkDF<-world[world$Country=='Denmark',]
#Create dataframe of Denmark with data from 1990 onwards
denmark1990DF<-denmarkDF[denmarkDF$year>=1990,]

#Create a variable that represents proportion of the population using the internet (internet users 
#divided by population).
denmarkInternet1990Prop<-denmark1990DF$internet.users/denmark1990DF$population

#Create a new variable that is "years since 1990". 
years1990onwardsDenmark<-denmark1990DF$year-1990

#Create a scatter plot of denmark internet use since 1990
plot(years1990onwardsDenmark,denmarkInternet1990Prop)

## Use a function to fit an exponential and logistic models for denmark internet use since 1990
expFit(years1990onwardsDenmark, denmarkInternet1990Prop)
logisticFit(years1990onwardsDenmark, denmarkInternet1990Prop)

#  In what YEARs does the exponential & logistic models predict that 70% of the Denmark population would be using the internet?
pred_exp <- expFitPred(years1990onwardsDenmark, denmarkInternet1990Prop, 16.08)
pred_log <- logisticFitPred(years1990onwardsDenmark, denmarkInternet1990Prop, 12.76)

# Research Question
#How has mobile phone usage in Brazil changed since 1995?

#Create dataframe with only with Brazil info.
BrazilDF<-world[world$Country=='Brazil',]

#Create a subset of the world bank data that contains records from Brazil 1995 and later.
Brazil1995DF<-BrazilDF[BrazilDF$year>=1995,]

#Change the year variable to be "years since 1995".
years1995onwardsBrazil<-Brazil1995DF$year-1995
#update the units of the "mobile.users" variable to millions of users.
Brazil_Mobile.mil <- Brazil1995DF$mobile.users / 1000000

#Create a scatter plot of the quantity of Brazil's mobile users since 1995
plot(years1995onwardsBrazil,Brazil_Mobile.mil)

## Fit a linear, exponential and logistic model to the quantity of Brazil's mobile users since 1995.
tripleFit(years1995onwardsBrazil,Brazil_Mobile.mil)

#Using the logistic, predict the number of mobile users (in millions) in Brazil in 2025.
logisticFitPred(years1995onwardsBrazil,Brazil_Mobile.mil, 30)
