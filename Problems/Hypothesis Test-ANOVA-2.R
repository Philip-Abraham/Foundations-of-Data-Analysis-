# A local police department has divided the city into three sections, and each is patrolled by a different 
# set of six (6) officers.  The police chief wants to determine if officers are biased in the number of 
# disorderly conduct tickets that they give out in each section.

# Here are the number of tickets given by the officers in each section in the last week:
setwd("C:/Users/Philip Abraham/OneDrive/TECHNOLOGY/Data Analysis/Data")
tickets_df <- read.csv(file="tickets.csv",head=TRUE,sep=",")
View(tickets_df)

#Calculate the mean for number of tickets in each section.
aggregate(ticket_qty~section,tickets_df,mean)

#Create a boxplot to check the assumptions of ANOVA.  Then run an ANOVA
boxplot(tickets_df$ticket_qty~tickets_df$section, main= "number of tickets Vs. section",ylab= "ticket quantity", xlab= "section")
aggregate(ticket_qty~section,tickets_df,sd)

# Run ANOVA
modeltick <- aov(tickets_df$ticket_qty~tickets_df$section)
summary(modeltick)

#Run a Tukey HSD post-hoc analysis if necessary
TukeyHSD(modeltick)