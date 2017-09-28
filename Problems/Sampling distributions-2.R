### Primary Research Question
## How many letters long is the typical UT student's name?  How does our estimate change as we increase 
## the size of our sample?


library(SDSFoundations)
data("StudentSurvey")
survey<-StudentSurvey
View(survey)

#How many of the first 10 students in the dataset had names longer than 5 letters?
dim(subset(survey, survey$ID<=10 & survey$name_letters>5))

#How long is the name of the first student in the dataset who is happy less than 40% of the time?
head(subset(survey, survey$happy<40),1)

#Determine the population parameters:
# 1. Visualize the shape of the population data by making a histogram.  
# 2. Calculate the "true" mean and standard deviation of the population.
hist(survey$name_letters)
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)

#Compare the sample statistics:  
# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =5)
xbar5[i] <-  mean(x)}


# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))


# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$name_letters)/sqrt(5)


#Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =15)
xbar15[i] <- mean(x)}
hist(xbar15,xlim=c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)


#Repeat for samples of size n=25
xbar25 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =25)
xbar25[i] <- mean(x)}
hist(xbar25,xlim=c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)

### Secondary Research Question

## What percentage of the time are college students happy?  How does our estimate of the true mean 
## change as sample size increases?

#Determine the population parameters:
# 1. Visualize the shape of the population data by making a histogram.  
# 2. Calculate the "true" mean and standard deviation of the population.
hist(survey$happy, main="Population")
fivenum(survey$happy)
mean(survey$happy)
sd(survey$happy)

#Compare the sample statistics:  
# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$happy, size =5)
xbar5[i] <-  mean(x)}


# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(0,100), main="n=5")


# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$happy)/sqrt(5)


#Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$happy, size =15)
xbar15[i] <- mean(x)}
hist(xbar15,xlim=c(0,100),main="n=15")
mean(xbar15)
sd(xbar15)
sd(survey$happy)/sqrt(15)


#Repeat for samples of size n=25
xbar25 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$happy, size =25)
xbar25[i] <- mean(x)}
hist(xbar25,xlim=c(0,100), main="n=25")
mean(xbar25)
sd(xbar25)
sd(survey$happy)/sqrt(25)


# On a scale of 1 to 10, how much do UT Austin students like Austin?
# 
# 1. What are the true mean and standard deviation for our population of UT Austin students?
# 
# 2. What should the sampling distribution of the mean look like, as predicted by the Central Limit Theorem?
# 
# 3. How do our simulated values compare to these predicted values?

# Create a histogram of the "austin" variable for the entire population of students that took the survey.
hist(survey$austin, main="Population Love Austin")
fivenum(survey$austin)
mean(survey$austin)
sd(survey$austin)
# Simulate drawing 1,000 random samples of sample size n=10 from the "austin" distribution, then create
# a histogram of the sampling distribution and calculate it's mean and standard deviation. 
xbar10 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$austin, size =10)
xbar10[i] <- mean(x)}
hist(xbar10,xlim=c(0,10),main="n=10")
mean(xbar10)
sd(xbar10)
sd(survey$austin)/sqrt(10)