# In 2011, researchers at the Texas Transportation Institute and the Center for Transportation Research 
# at UT Austin ran an advertising campaign aimed at recruiting Austin cyclists to join the South Congress 
# Bike Mapping Project.  As members of the project, cyclists downloaded and used Cycle Tracks, 
# a smartphone app developed by the San Francisco County Transportation Authority to track where people 
# are riding their bikes based on their GPS points.  The goal was to gain new information about bike 
# commuting patterns and this data set is based on the results of the study: 3600 trips tracked 
# from 315 users over a 6 month period.   Data includes distances traveled, speed of travel, and 
# reasons for travel among other variables.

bike<-BikeData
males <- bike [bike$gender == 'M',]
male_times <- males$time
mean(male_times)

#how many daily riders are in the original dataset?
daily_riders<-bike[bike$cyc_freq == 'Daily',]

#How many of the daily riders are male?
male_Daily_riders<-daily_riders[daily_riders$gender== "M",]

#What is the average age of the male daily riders?
MeanAge_male_Daily_riders<-mean(male_Daily_riders$age)

#How many of the daily riders are female?
female_Daily_riders<-daily_riders[daily_riders$gender== "F",]

#What is the average age of the female daily riders?
MeanAge_female_Daily_riders<-mean(female_Daily_riders$age)


# What is the average age of daily riders?
MeanAge_daily_riders<-mean(daily_riders$age)

#How many daily male riders are age 30 or older?
AgeThirty_male_riders<-male_Daily_riders[male_Daily_riders$age>=30,]

