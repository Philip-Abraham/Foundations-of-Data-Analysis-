# Primary Research Question
# 
# 1. Do students at UT spend more time on homework per week in college than they did in high school?

library(SDSFoundations)
data("PostSurvey")
post<-PostSurvey
View(post)

#Create vectors of the scores that you wish to analyze.
HS_HW<-post$hw_hours_HS
CO_HW<-post$hw_hours_college
#Check the assumption of normality by generating a histogram for each variable of interest.
hist(HS_HW,main="High Schoool HW Hours")
hist(CO_HW,main="College HW Hours")
hist((CO_HW-HS_HW),main="Difference HW Hours")
#On average, students spent how many hours more on homework each week in college than they did in high school?
mean(CO_HW)-mean(HS_HW)
#Find the t-statistic and p-value
t.test(CO_HW,HS_HW,paired = TRUE)

# Secondary Research Question
#
# 2. Do students in fraternities and sororities get less sleep on the weekends than other college students?

#Create vectors of the scores that you wish to analyze.
FR_Sleep<-post$sleep_Sat[post$greek=='yes']
NOFR_Sleep<-post$sleep_Sat[post$greek=='no']
#On average, students who are Greek sleep how many hours less than Non-Greek students on Saturday nights? 
mean(FR_Sleep)-mean(NOFR_Sleep)
#Check the assumption of normality by generating a histogram for each variable of interest.
hist(FR_Sleep,main="Greek Sleep Hours")
hist(NOFR_Sleep,main="Non-Greek Sleep Hours")
#Find the t-statistic and p-value
t.test(FR_Sleep,NOFR_Sleep,alternative = 'less')


# Final Research Question
#
#Is the increase in time spent studying from high school to college the same for nursing majors and biology majors?

#Create vectors of the scores that you wish to analyze.

##HIGH SCHOOL
Nur_HS_Stdy<-post$hw_hours_HS[post$major=='Nursing']
Bio_HS_Stdy<-post$hw_hours_HS[post$major=='Biology']

##COLLEGE
Nur_COL_Stdy<-post$hw_hours_college[post$major=='Nursing']
Bio_COL_Stdy<-post$hw_hours_college[post$major=='Biology']

##DIFFERENCES
Diff_Nur_Stdy<-Nur_COL_Stdy-Nur_HS_Stdy
Diff_Bio_Stdy<-Bio_COL_Stdy-Bio_HS_Stdy

#Check the assumption of normality by generating a histogram for each variable of interest.
hist(Diff_Nur_Stdy,main='Study Difference - Nursing')
hist(Diff_Bio_Stdy,main='Study Difference - Biology')

#Find the t-statistic and p-value
t.test(Diff_Nur_Stdy,Diff_Bio_Stdy)
