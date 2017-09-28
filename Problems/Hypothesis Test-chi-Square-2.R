## Primary Research Questions

## 1. Are there an equal number of male and female performers on Austin City Limits?

library(SDSFoundations)
View(AustinCityLimits)
acl <- AustinCityLimits

# Create a table of counts for Gender
gender_tab <-table(acl$Gender)
gender_tab

# Create vector of expected proportions
ExpGender <- c(.50, .50)

# Check expected counts assumption
chisq.test(gender_tab, p=ExpGender)$expected

# Run goodness of fit
chisq.test(gender_tab, p=ExpGender)
#Based on the chi-GOF test should reject the hypothesis that there were an equal number of male and 
#female performers at ACL Live.

## 2. Are male performers just as likely to have had a Top 10 hit as female performers?

# Create two-way table
gender_top10 <-table(acl$Gender, acl$BB.wk.top10)
gender_top10

# Generate expected counts
chisq.test(gender_top10, correct=FALSE)$expected

# Run test of independence
chisq.test(gender_top10, correct=FALSE)
#Based on test of independence score, we should fail to reject the hypothesis that gender is not 
#associated with having a Top 10 hit.


## Secondary Research Questions

## 1. Are each of the four musical genres equally represented on Austin City Limits?   

#Create a table to show the counts of each genre.
genre_table<-table(acl$Genre)
genre_table

#Create a vector of expected proportions.
exp_genre<-c(1/4,1/4,1/4,1/4)

#Check the expected counts assumption.
chisq.test(genre_table, p=exp_genre)$expected

# Run a chi square test
chisq.test(genre_table, p=exp_genre)
#Based on the chi-GOF test should reject the hypothesis that each of the four musical genres were equally 
#represented on Austin City Limits.


## 2. Are some genres more likely to draw a large (100K+) Twitter following than others?

#Create a two-way table for genre and Twitter following.
Twit100K_Genre<-table(acl$Genre,acl$Twitter.100k)
Twit100K_Genre

#Check the expected counts assumption.
chisq.test(Twit100K_Genre, correct=FALSE)$expected

#Run a chi square test.
chisq.test(Twit100K_Genre, correct=FALSE)
##Based on test of independence score, we should fail to reject the hypothesis that genre is 
##independent of Twitter followers.


# You want to know if the proportion of female performers on Austin City Limits Live has changed in the 
# past two years. 
# 
# 1. Create a new variable in the dataset called "Recent" that is equal to a 1 for rows from years 2012 
# or 2013 and is equal to 0 for all other rows.
acl$Recent[acl$Year < 2012] <- 0 
acl$Recent[acl$Year >= 2012] <- 1
View(acl)

#How many female performers have been on the show in the past two years (2012 and 2013)?
table(acl$Recent[acl$Gender=="F"])

# Create two-way table
count_table <-table(acl$Gender, acl$Recent)
count_table

#Report expected counts for the following groups: 
# Females before 2012 group
# Females in 2012 and 2013
# Males before 2012
# Males in 2012 and 2013
chisq.test(count_table, correct=FALSE)$expected

#Use the Chi-Square Test of Independence to test if representation by female performers is different 
#before 2012 compared to since 2012?
chisq.test(count_table, correct=FALSE)
#Based on test of independence score, we fail to reject the null hypothesis; gender is independent of
#performance before or after 2012. We fail to reject the null hypothesis; gender is independent of 
#performance before or after 2012. - correct
