#Primary Research Question
#How long do animals stay in the shelter before they are adopted?

animaldata <- AnimalData
head(animaldata)
names(animaldata)

#How many variables are in this dataset?
str(animaldata)

# How many of the first 10 animals in the dataset were adopted?
adopted<-animaldata$Outcome.Type[1:10]=="Adoption"
table(adopted)

#Was the first owner-surrendered animal in the dataset neutered?
intaketype<-animaldata$Intake.Type=='Owner Surrender'
first_neutered_ownersurr<-intaketype[animaldata$Neutered.Status=='Neutered']
first_neutered_ownersurr[1]

#Find the number of animals that were adopted
table(animaldata$Outcome.Type)

#Pull out only adopted animals
adopted <- animaldata[animaldata$Outcome.Type=="Adoption",]

#Pull out just the days in shelter for the adopted animals
daystoadopt <- adopted$Days.Shelter
#It looks like one adopted animal spent much more time in the shelter than the others.
#How many days was this animal in the shelter?
max(daystoadopt)

#Visualize and describe these variables
hist(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)

#Tell me something about the animal that to the max time to adopt from shelter
animaldata[which(animaldata$Days.Shelter==max(daystoadopt)),]

#Subset the dataset for only male animals
animaldata<-AnimalData 
males<-animaldata[animaldata$Sex == 'Male',] 

#Secondary Research Question
#Compare the weight of adult cats and dogs at the shelter.
#How typical would it be to find a 13-pound cat?  What about a 13-pound dog?

#Create a table to show how many adult (at intake) cats and dogs are in the dataset.
#An animal is considered an adult if it is at least one year of age.

dogs<-animaldata[animaldata$Animal.Type=='Dog',]
cats<-animaldata[animaldata$Animal.Type=='Cat',]

#How many adult dogs and cats are in the shelter?
adultdogs<-dogs[dogs$Age.Intake>=1,]
adultcats<-cats[cats$Age.Intake>=1,]

#Create histograms for the distribution of weights for adult dogs and cats
hist(adultdogs$Weight,xlab='Adult Dog Weight',breaks=15)
hist(adultcats$Weight,xlab='Adult Cat Weight',breaks=15)

#Average adult cat weight
mean(adultcats$Weight)

#Standard deviation for the weight of the adult cats
sd(adultcats$Weight)

# z-score of a 13 pound adult cat
z_adultcat_13<-(13-mean(adultcats$Weight))/sd(adultcats$Weight)

#What proportion of adult cats weigh more than 13 pounds?
1-pnorm(z_adultcat_13)

#Five Number Summary for adult dog weights
fivenum(adultdogs$Weight)

#What was the most common way that dogs arrived in the shelter?
table(animaldata$Intake.Type)

#What proportion of dogs were brought to the shelter as an owner surrender?
ownsurrdogs<-dogs$Intake.Type[dogs$Intake.Type=='Owner Surrender']
table(ownsurrdogs)
table(dogs$Intake.Type)

#Of the dogs that were brought to the shelter as an owner surrender,
#how many were returned to their owner?
ownsurrdogs<-dogs[dogs$Intake.Type=='Owner Surrender',]
ownsurrdogs_Return_to_owner<-ownsurrdogs$ Outcome.Type[ownsurrdogs$ Outcome.Type=='Return to Owner']
table(ownsurrdogs_Return_to_owner)

#What was the mean number of days that the above dogs referenced  spent at the shelter before being returned to their owner?
ownsurrdogs_Return_to_owner<-ownsurrdogs[ownsurrdogs$ Outcome.Type=='Return to Owner',]
mean(ownsurrdogs_Return_to_owner$Days.Shelter)
