library(SDSFoundations)
data("BullRiders")
bull<-BullRiders
View(bull)

# Primary Research Question
# The average American adult man weighs 190 pounds.  Do professional bull riders from the US weigh the same?
# We will use a one-sample t-test to compare the average weight of these bull riders to a claimed value.
# The null hypothesis look like for this one-sample t-test: ??=190 pound

#Select bull riders from the US
USA <-bull[bull$Country=="USA",]

# Summarize the bull rider weights
mean(USA$Weight)
sd(USA$Weight)

# Visualize the weight distribution
hist(USA$Weight, main='Histogram of US Bull Rider Weights',xlab='Weight (lbs)')

# Run the single sample t-test
t.test(USA$Weight, mu=190)
# The p-value (< 2.2e-16) of the test was very small (< 0.05)
#If bull-riders really do weigh 190 pounds on average, observing this sample mean is very unlikely.

#The distribution of weight for this sample of professional bull riders is   
# with a mean of 153.11 lbs and a   
# sd  of 13.02 lbs. We found that their mean weight   
# is significantly different than 190 lbs, with t= -17.2, p   
# less than  0.05. We are   
# we are 95%  confident that the true mean of professional bull riders is between 148.8 lbs and 157.5 lbs, suggesting that professional bull riders weigh   
# less than  the average adult male.

# Secondary Research Question
# Do professional bull riders stay on their bulls 50% of the time? Test the hypothesis that the mean ride
# percentage is 0.500 in 2014, using riders with at least 5 events in 2014. 

#Select the riders that participated in at least 5 events in 2014.
riders5_2014<-subset(bull, bull$Events14>=5)
#Calculate the sample mean and standard deviation of ride percentage in 2014.
mean(riders5_2014$RidePer14)
sd(riders5_2014$RidePer14)
#Generate a histogram to look at the distribution of the ride percentage in 2014.
hist(riders5_2014$RidePer14,main="distribution of the ride percentage in 2014")

#What is the value of the t-statistic?
t.test(riders5_2014$RidePer14, mu=0.500)
# The distribution of the percentage of time a professional bull rider stays on the bull for this sample is  
# approximately normal,with a mean of 33.50%, and a   standard deviation 10.7%. 
# We found that their mean ride percentage is significantly different from 50%, with t= -10.05, p<0.05. 
# We are 95% - This answer is correct.  confident that the true mean of ride percentage of professional
# bull riders is between 30.1% and 36.8% suggesting that professional bull riders ride the full 8 seconds about 1/3 of the time.


## How much money do professional bull riders earn by participating in an event?

#Create a new variable that equals the "average earnings per event" in the 2012 season for each bull rider in the dataset. 
earnings_per<-bull$Earnings12/bull$Events12
#Make a histogram of your "earnings per event" variable.
hist(earnings_per,main="earnings per event-2012") #the distribution of "earnings_per" is positively skewed, with an outlier
#When a variable is highly skewed, we can transform the data into a shape that allows us to conduct our analysis. 
#Create a new variable that is the log of your "earnings_per" variable
log_earnings_per <- log(earnings_per)
#Make a histogram of this log-transformed variable. Notice how the distribution shape has changed. 
#We can now reliably calculate a 95% confidence interval for the mean of this transformed variable.
hist(log_earnings_per,main="log of earnings per event-2012")
mean(log_earnings_per, na.rm = TRUE)

#What is the value of the t-statistic?
t.test(log_earnings_per, mu=8.85)
#Change log-transformed value back to the original units.
exp(8.572169)

exp(9.120605)