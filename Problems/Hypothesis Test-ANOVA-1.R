library(SDSFoundations)
film <- FilmData
View(film)

boxplot(film$Days,main='Days in the Theater')
boxplot(film$Days~film$Genre,main='Days in the Theater', xlab='All Films',ylab='Days')

#To get each group means
aggregate(Days~Genre,film,mean)

#to test the one-way ANOVA assumption that all groups should have roughly the same variances:
aggregate(Days~Genre,film,sd)

#ANOVA Function
daysmodel<-aov(film$Days~film$Genre)
summary(daysmodel)

#If there was a significant difference between the groups, then you run POST-HOC Analysis
TukeyHSD(daysmodel)

# What is the name of the highest ranked film made by Universal Studios?
r<-min(film$Rank[film$Studio=='Uni.'])
film$Film[film$Rank==r]

#What was the lowest IMDB rating given to a film that ranked in the top 10 grossing films of all time?
min(film$IMDB[film$Rank<=10])


# Primary Research Questions
# 
##### 1. Does a film's rating (PG, PG-13, or R) impact its cost to produce?

# Show how many films are in each group
table(film$Rating)

# Calculate avg film budget of each group
aggregate(Budget~Rating,film,mean)

# Calculate sd of film budget within each group
aggregate(Budget~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$Budget~film$Rating, main= "Film Budgets by Rating",ylab= "Budget", xlab= "MPAA Rating")

# Run ANOVA
modelbud <- aov(film$Budget~film$Rating)
summary(modelbud)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelbud)


##### 2. Does a film's rating (PG, PG-13, or R) influence its IMDB score?

# Calculate avg IMDB score of each group
aggregate(IMDB~Rating,film,mean)

#Calculate sd of IMDB scores within each group
aggregate(IMDB~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$IMDB~film$Rating, main= "IMDB Scores by Rating",
        ylab= "IMDB Score", xlab= "MPAA Rating")

# Run ANOVA
modelscore <- aov(film$IMDB~film$Rating)
summary(modelscore)

# Run post-hod text if F statistic is significant
TukeyHSD(modelscore)


# Secondary Research Questions
# 
###### 1. Are some studios more successful in keeping their films in the theaters longer?

#The number of top-grossing films produced by each studio were:
table(film$Studio)

# Calculate avg number of days each film stayed in theater for each of the studios
aggregate(Days~Studio,film,mean)

#to test the one-way ANOVA assumption that all groups should have roughly the same variances:
aggregate(Days~Studio,film,sd)

# Visualize the group means and variability
boxplot(film$Days~film$Studio, main= "Number of Days in Theater",ylab= "Days", xlab= "Studio")

# Run ANOVA
modeldays <- aov(film$Days~film$Studio)
summary(modeldays)

# Run post-hod text if F statistic is significant
TukeyHSD(modeldays)


###### 2. Do some studios earn a greater percentage of their earnings domestically than others?

# Calculate average percentage of domestic earnings for each of the studios
aggregate(Pct.Dom~Studio,film,mean)

#to test the one-way ANOVA assumption that all groups should have roughly the same variances:
aggregate(Pct.Dom~Studio,film,sd)

# Visualize the group means and variability
boxplot(film$Pct.Dom~film$Studio, main= "proportion of domestic earnings",ylab= "proportion", xlab= "Studio")

# Run ANOVA
modelprop <- aov(film$Pct.Dom~film$Studio)
summary(modelprop)

# Run post-hod text if F statistic is significant
TukeyHSD(modelprop)


#########Do low-budget movies make a different percentage of their profits domestically than movies 
#########with medium- or high-budgets? 

# Suppose films with a budget less than $100 Million are considered "low-budget"; films with a budget 
# of $100-150 Million are considered "medium-budget", and those with a budget of $150 million or more 
# are "high-budget."  
# 
# Create a new categorical variable in the dataset that defines each film under these criteria.
for(i in 1:151) {
        
        a<-is.na(film$Budget[i])
        
        if(a=='TRUE')  {
                
                film$Budget_Cat[i]<-c("NA")
        
                }       else if(film$Budget[i]>=150)    {
                film$Budget_Cat[i]<-c("high-budget")
        
                }       else if(film$Budget[i]<150 & film$Budget[i]>=100){
                film$Budget_Cat[i]<-c("medium-budget")
        
                }       else    {
                film$Budget_Cat[i]<-c("low-budget")
        }
}
        
# Revise film dataframe to remove NA's
film_rev <- film[complete.cases(film), ]

#How many films fall into each of the budget categories?
table(film_rev$Budget_Cat)

#Calculate the mean percent domestic for each group
aggregate(Pct.Dom~Budget_Cat,film_rev,mean)

#Create a boxplot to check the assumptions of ANOVA.  Then run an ANOVA
boxplot(film_rev$Pct.Dom~film_rev$Budget_Cat, main= "proportion of domestic earnings Vs. Budget",ylab= "proportion", xlab= "Budget Category")
aggregate(Pct.Dom~Budget_Cat,film_rev,sd)

# Run ANOVA
modelbudg <- aov(film_rev$Pct.Dom~film_rev$Budget_Cat)
summary(modelbudg)

#Run a Tukey HSD post-hoc analysis
TukeyHSD(modelbudg)
