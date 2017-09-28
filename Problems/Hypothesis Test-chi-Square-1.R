# A major snack food company claims that its chips are "America's favorite." A statistics class tests this 
# claim by asking a sample of 90 random students on campus to select their favorite chip from the 
# company's (Brand A) and two other brands (Brand B and Brand C). 
# Below are the results of how many students selected each brand in their taste test.

Brand_A<-38
Brand_B<-28
Brand_C<-24

chips_df<-data.frame(Brand_A,Brand_B,Brand_C)

chisq.test(chips_df) ##Chi-square statistic
#to check the assumption of sample size sufficiency you add $expected at the end of previous expression
chisq.test(chips_df)$expected


# If the expected value for each brand was:
exp<-c(0.20,0.50,0.3)
chisq.test(chips_df,p=exp) ##Chi-square statistic
#to check the assumption of sample size sufficiency you add $expected at the end of previous expression
chisq.test(chips_df,p=exp)$expected

# If the expected value for each brand was:
exp<-c(1/3,1/3,1/3)
chisq.test(chips_df,p=exp) ## is the same as just saying: chisq.test(chips_df)
#to check the assumption of sample size sufficiency you add $expected at the end of previous expression
chisq.test(chips_df,p=exp)$expected

######################################################################################################

# Jurors are selected from the list of registered voters, so the ages for jurors should have the same 
# distribution as the ages of voters. A law professor obtains voter registration records and finds that 
# 20% of registered voters are 18-29 (Young), 45% are 30-49 (Middle), and 35% are age 50 (Old) or older. 
Young_Voter<-0.20
Middle_Voter<-0.45
Old_Voter<-0.35
exp<-c(Young_Voter, Middle_Voter,Old_Voter)

# The professors then monitors jury composition over a month-long period and finds the following distribution of jurors:  
Young_Jury<-12
Middle_Jury<-36
Old_Jury<-32
jury_df<-data.frame(Young_Jury,Middle_Jury,Old_Jury)

chisq.test(jury_df, p=exp) ##Chi-square statistic
#to check the assumption of sample size sufficiency you add $expected at the end of previous expression
chisq.test(jury_df, p=exp)$expected
######################################################################################################

#Here is the data from a research study on the relationship between sex and fear of heights.

Men<-c(68,94)
Women<-c(109,89)

FOH_df<-data.frame(Men,Women)
rownames(FOH_df)<-c("Fear_Heights","No_Fear_Heights")

##Chi-square statistic.
# correct= FALSE.....a logical =FALSE indicating not to apply continuity correction when computing the test statistic for 2 by 2 tables: 
chisq.test(FOH_df,correct= FALSE)
#to check the assumption of sample size sufficiency you add $expected at the end of previous expression
chisq.test(FOH_df,correct= FALSE)$expected

######################################################################################################

# When crossing white and yellow summer squash, a genetic model predicts that 75% of resulting offspring 
# will be white, 15% will be yellow and 10% will be green. 
exp<-c(0.75,.15,.10)

# Below are the results from an experiment run 
# on a random sample of 205 squash offspring.
White<-152
Yellow<-39
Green<-14
squash_df<-data.frame(c(White),c(Yellow),c(Green))
rownames(squash_df)<-c("Number of Offspring")

#What are the expected counts?
chisq.test(squash_df, p=exp)$expected
#What is the Chi Square statistic for this test?
chisq.test(squash_df, p=exp)

#####################################################################################################
# Approximately 13% of the world's population is left-handed, but is this proportion the same across men 
# and women? To answer this question, you decide to collect data from a random sample of adults from your
# neighborhood, with the following results:
setwd("C:/Users/Philip Abraham/OneDrive/TECHNOLOGY/Data Analysis/Data")
hand_df <- read.csv(file="gender-hand.csv",head=TRUE,sep=",")

# The Null Hypothesis is Gender and hand-dominance are independent. Gender and hand-dominance are independent
hand_table<-table(hand_df$Gender,hand_df$Dominant.Hand)
hand_table
# What are the expected counts for Males & Females?
exp<-c(.13,.87)
chisq.test(hand_table, p=exp,correct= FALSE)$expected

######################################################################################################
# A telephone survey asked a random sample of Indiana voters about their home internet usage, as well as
# what type of community (rural, suburban or urban) they lived in. Of the 123 survey respondents, 28 
# were from rural areas, 42 were from suburban areas, and 53 were from urban areas.  Thirteen rural 
# respondents, 35 suburban respondents, and 50 urban respondents said they had access to internet at home. 
setwd("C:/Users/Philip Abraham/OneDrive/TECHNOLOGY/Data Analysis/Data")
internet_df <- read.csv(file="internet_usage.csv",head=TRUE,sep=",")
rownames(internet_df)<-c("Internet_Yes", "Internet_No")
internet_df

# The Null Hypothesis is  home internet access and community type are independent.
# What are the expected counts for internet usage?
chisq.test(internet_df)$expected
#What is the Chi Square statistic for this test?
chisq.test(internet_df)
