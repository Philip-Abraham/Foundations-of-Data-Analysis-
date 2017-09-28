# Does chewing gum make you less accurate while target shooting?  Each of the following subjects shot at a 
# target (to earn a maximum score of 100) in random order, once while chewing gum and once while not chewing.
# Assume all test conditions are met.         
Chewing_Gum<-c(79,95,85,82)
No_Gum<-c(80,94,87,84)

t.test(Chewing_Gum,No_Gum, paired = TRUE)

diff_twogroups<-Chewing_Gum-No_Gum
mean(diff_twogroups)
sd(diff_twogroups)
SE<-sd(diff_twogroups)/sqrt(length(Chewing_Gum))
#################################################################################################

library(SDSFoundations)
data("PostSurvey")
post<-PostSurvey
View(post)

#Of the first 10 students in the dataset, what percentage live on campus?
FirstTen<-post[1:10,]
quantityFirstTen<-dim(FirstTen)
live<-dim(subset(FirstTen,FirstTen$live_campus=="yes"))
(live[1]/quantityFirstTen[1])*100

# Primary Research Questions

# 1.  Who is happier at the beginning of the semester:  under-classmen or upper-classmen?

# Make a vector of happiness scores for each sample
underclass_happy <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass_happy <- post$happy[post$classification=='Junior'|post$classification=='Senior']

# Check the normality assumption
hist(underclass_happy, xlab='Underclassman Happiness', main='Percent of Time Happy')
hist(upperclass_happy, xlab='Upperclassman Happiness', main='Percent of Time Happy')

# Run independent t-test
t.test(underclass_happy, upperclass_happy)

# 2.  Does student happiness change from the beginning of the semester to the end?

# Make a vector of difference scores
post$diff_happy <- post$happy - post$post_happy

# Check the normality assumption
hist(post$diff_happy, xlab= 'Difference in Happiness over the Semester', main = 'Happy-Post Happy')

# Run dependent t-test
t.test(post$happy, post$post_happy, paired=T)


#Suppose we wanted to test the happiness scores of those who live on campus against those who live off campus. 
on_campus <- post[post$live_campus == 'yes',]
off_campus <- post[post$live_campus == 'no',]
on_campus_happy <- on_campus$happy
off_campus_happy <- off_campus$happy
t.test(on_campus_happy, off_campus_happy)




