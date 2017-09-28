#Primary Research Question
#For the 2013 season, Is there a linear relationship between how often a rider placed in the Top 10 
#and the number of times he stayed on his bull for a full 8 seconds?

library(SDSFoundations)

bull <- BullRiders

# How many of the first 10 riders in the dataset have been pro for 10 years or more?
YearsPro10<-bull[1:10,"YearsPro"]
YearsPro10_10<-YearsPro10>=10
table(YearsPro10_10)

#Of the top 15 riders so far in 2015, 
#how many rides were completed by the rider with the fewest buck-outs in 2014?
bullTop15<-bull[1:15,]
bullTop15_min2014BO<-min(bullTop15$BuckOuts14)
bullTop15_min2014BO_Rides14<-bullTop15$Rides14[bullTop15$BuckOuts14==bullTop15_min2014BO]

#Subset for riders that participated in at least one event in 2013
new_bull <- bull[bull$Events13  > 0 ,]

# Visualize and describe the first variable of interest
hist(new_bull$Rides13,breaks=10)
fivenum(new_bull$Rides13)
mean(new_bull$Rides13)
sd(new_bull$Rides13)

# Visualize and describe the second variable of interest
hist(new_bull$Top10_13)
fivenum(new_bull$Top10_13)
mean(new_bull$Top10_13)
sd(new_bull$Top10_13)

# Create a scatterplot
plot(new_bull$Rides13,new_bull$Top10_13)

# Add line of best fit
abline(lm(new_bull$Top10_13~new_bull$Rides13))

# Calculate the correlation coefficient
cor(new_bull$Rides13,new_bull$Top10_13)

# Create a correlation matrix 
vars <- c("Top10_13", "Rides13")
cor(new_bull[,vars])

#identify a specific record
which(new_bull$Top10_13==2 & new_bull$Rides13==22)


#Secondary Research Question - In 2012, which variable had the strongest linear relationship 
#with Earnings:  Ride Percentage or Cup Points?

#Create a dataset which contains riders that participated in at least one event in 2012
new_bull12<-bull[bull$Events12>=1,]

#Make a histogram to visualize the distribution of Earnings for 2012.
hist(new_bull12$Earnings12)

#measure of center
fivenum(new_bull12$Earnings12)

#Make a Scatterplot of Earnings and Ride Percentage
plot(new_bull12$RidePer12,new_bull12$Earnings12)
abline(lm(new_bull12$Earnings12~new_bull12$RidePer12))

#What is the correlation of Earnings with Ride Percentage for 2012?
cor(new_bull12$Earnings12,new_bull12$RidePer12)

#Remove the outlier data point from the dataset to assess what kind of 
#impact, if any, it had on our correlation analysis. 

# identify specific case-identify the extreme data value in Earnings:
which(new_bull12$Earnings12 == max(new_bull12$Earnings12))
#Subset the data
nooutlier <- new_bull12[new_bull12$Earnings12 < 1000000 ,] 
plot(nooutlier$RidePer12,nooutlier$Earnings12)
abline(lm(nooutlier$Earnings12~nooutlier$RidePer12))
#What is the new correlation of Earnings with Ride Percentage for 2012?
cor(nooutlier$Earnings12,nooutlier$RidePer12)

#Create a Scatterplot of Earnings and Cup Points
plot(new_bull12$CupPoints12,new_bull12$Earnings12)
abline(lm(new_bull12$Earnings12~new_bull12$CupPoints12))
#plot with outlier removed
plot(nooutlier$CupPoints12,nooutlier$Earnings12)

#What is the correlation of Earnings with Cup Points for 2012?
cor(new_bull12$Earnings12,new_bull12$CupPoints12)
#Correlation with outlier removed
cor(nooutlier$Earnings12,nooutlier$CupPoints12)

#Subset the dataset for riders that had at least 1 ride in the 2014 season
new_bull<-bull[bull$Rides14>=1,]

#Create a new variable or vector for the average number of rides per 
#event for each bull rider in the new_bull dataset:
RidesPerEvent14 <- new_bull$Rides14/new_bull$Events14

#Make a histogram of your "rides per event" variable and find the 
#five-number summary for your "rides per event" variable:
hist(RidesPerEvent14)
fivenum(RidesPerEvent14)

#Create a scatterplot of "rides per event" and 
#yearly ranking (defined by the "Rank14" variable) and add a line of best fit.
Rank14<-new_bull$Rank14
plot(RidesPerEvent14,Rank14)
abline(lm(Rank14~RidesPerEvent14))

#What is the correlation coefficient for rides per event and yearly ranking? 
cor(Rank14,RidesPerEvent14)
