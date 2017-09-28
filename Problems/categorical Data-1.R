#Primary Research Question
#For artists age 30 or older, do female artists play different kinds of music on 
#Austin City Limits than male artists?


library(SDSFoundations)

acl<-AustinCityLimits
#How many of the first 10 artists in the dataset were Grammy winners?
acl10<-acl[1:10,]
table(acl10$Grammy=="Y")

#What genre was played by the first female artist in the dataset who was over 60 years of age?
aclF<-acl[acl$Gender=="F",]
aclF60<-aclF[aclF$Age>=60,]
aclF60[1,]

# To get a proportion for a table
gtab<-table(acl$Grammy)
prop.table(gtab)

#Build of Contigency Table
gtab2<-table(acl$Grammy,acl$Gender)

#Proportion table for totals
prop.table(gtab2)

#Conditional Probability - Proportion accross rows to add to 1
prop.table(gtab2,1)

#Conditional Probability - Proportion accross columns to add to 1
prop.table(gtab2,2)

#STACKED Bar Chart with legend
barplot(gtab2,legend=T,xlab='Gender',ylab='Counts',main='Gender by Grammy Winners')

#SIDE BY SIDE Bar Chart with legend
barplot(gtab2,legend=T,xlab='Gender',ylab='Counts',main='Gender by Grammy Winners',beside=T)

# To get a relativistic %  plot
barplot(prop.table(gtab2,2),legend=T,xlab='Gender',ylab='% Counts',main='Gender by Grammy Winners')

#Subset the data for artists age 30 or older
older <-acl[acl$Age>=30,]

# Create tables of marginal distributions
genre <- table(older$Genre)
genre
gender <- table(older$Gender)
gender

# Create contingency table 
twoway <- table(older$Gender,older$Genre)
twoway

# Visualize the counts
barplot(twoway, legend=T, beside=T)

# Calculate P(A): the probability of each genre being played
prop.table(genre)

# Calculate P(A|B): the probability of each genre being played, given the artist's gender
prop.table(twoway,1)



#Secondary Research Question
#Among male artists, is there an association between winning a 
#Grammy and the genre of music that he plays?

#Subset the data (males only)
aclM<-acl[acl$Gender=='M',]
#How many male artists did and did not win a Grammy?
table(aclM$Grammy=='Y')
#Which genre had the greatest number of Grammy wins?
Country<-which(aclM$Grammy=='Y' & aclM$Genre=='Country')
Rock<-which(aclM$Grammy=='Y' & aclM$Genre=='Rock/Folk/Indie')
Jazz<-which(aclM$Grammy=='Y' & aclM$Genre=='Jazz/Blues')
Singer<-which(aclM$Grammy=='Y' & aclM$Genre=='Singer-Songwriter')


# Create tables of marginal distributions
grammyW <- table(aclM$Grammy=='Y')
grammyW
genreM <- table(aclM$Genre)
genreM

tworowTable<-table(aclM$Genre,aclM$Grammy=='Y')
tworowTable

# What is the probability that a randomly selected artist was a Grammy winner?
prop.table(grammyW)

prop.table(tworowTable)
prop.table(tworowTable,1)
prop.table(tworowTable,2)

#Plots
barplot(grammyW, legend=T, beside=T)
barplot(prop.table(grammyW), legend=T, beside=T)
barplot(tworowTable, legend=T, beside=T)
barplot(prop.table(tworowTable), legend=T, beside=T)
barplot(prop.table(tworowTable,1), legend=T, beside=T)
barplot(prop.table(tworowTable,2), legend=T, beside=T)

#You want to see if an artist's popularity on Facebook (whether or not they have 100,000 or 
#more likes) has anything to do with their age.

#Generate a table to show the number of artists that are "popular" and those that are not.
PopArt<-acl[acl$Facebook.100k==1,]
NonPopArt<-acl[acl$Facebook.100k==0,]
POPandNON<-table(acl$Facebook.100k==1,acl$Facebook.100k==0)

#Generate a table to show the number of "popular" artists within each age group.
AgeTablePop<-table(PopArt$Age.Group)
#Generate a table to show the number of "all" artists within each age group.
AgeTable<-table(acl$Age.Group)

# For each age group, what is the proportion of artists who have 100,000 or more likes on Facebook?
AgeTablePop/AgeTable
