library(SDSFoundations)
res <- TempskiResilience
library(psych)
View(res)

# Primary Research Questions
# 
###### 1) Can you confirm the claim that Beck Depression Inventory score is a significant predictor of Overall 
###### Quality of Life among students enrolled in the Clinical Sciences program?

#Subset into the Clinical Sciences
clin <- res[res$Group == "Clinical Sciences",]

#Intial Correlations
##What is the inital correlation coefficient between overall Quality of Life and Beck Depression Inventory? 
vars <- c("QoL", "BDI")
cor(clin[,vars])

# What is the t-value of the Simple Regression slope for Beck Depression Inventory? 
#Run the model for Research Question 1 and examine statistics
ov_mod <- lm(QoL ~ BDI, data=clin)
summary(ov_mod)
confint(ov_mod)
##Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff,id.n=6)


###### 2) For students enrolled in the Clinical Sciences program, examine the effects of DREEM: Social
######Self Perception, DREEM: Academic Self Perception, Resilience, BDI, and Age on Med School Quality 
######of Life? 

#Initial correlations
vars <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clin[,vars], use="pairwise.complete.obs")

# Run the model for Research Question 2 and examine.
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data=clin)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context-contextual analysis
lmBeta(ms_mod) 
round(pCorr(ms_mod), 4) 

#######PRIMARY RESEARCH QESTION CONCLUSIONS
# To answer our primary research questions about Clinical Sciences med students, we conducted two primary
# tests: a simple linear regression and a multiple linear regression. First, we investigated the claimed 
# effect of Beck Depression Inventory score on Overall Quality of Life. There was a negative correlation
# between Beck Depression Inventory score and Overall Quality of Life. The corresponding model showed a 
# significant simple slope for Beck Depression Inventory (t(489)=  -8.935, p<0.05) indicating that as the
# Beck Depression Inventory score increases, the Overall Quality of Life score decreases
# 
# The multiple linear regression examined the impact of DREEM: Social Self Perception, DREEM: Academic 
# Self Perception, Resilience, BDI, and Age on Med School Quality of Life. Overall, the model was 
# significant (F(5,485)= 48.59, p<.05), and could account for 33.37% of the variance in the 
# outcome (Adjusted R2=32.69%). The best predictor of Med School Quality of Life was DREEM: Social Self 
# Perception which could account for a unique proportion of variance in the outcome of 8.23%.


# SECONDARY RESEARCH QUESTIONS
# 
# 1. For students in the Basic Sciences program, of the four measures of Quality of Life (Physical Health, 
# Psychological, Social Relationships, and Environment), which has the greatest impact on Med School 
# Quality of Life?

# Subset the data for students in the Basic Sciences Program.
basc <- res[res$Group == "Basic Sciences",]

# Determine the variables of interest and the outcome variable.
vars <- c("MS.QoL","WHOQOL.PH", "WHOQOL.PSY", "WHOQOL.SOC", "WHOQOL.ENV")
cor(basc[,vars], use="pairwise.complete.obs")

# Run a Multiple Linear Regression.
ms_mod2 <- lm(MS.QoL ~ WHOQOL.PH + WHOQOL.PSY + WHOQOL.SOC + WHOQOL.ENV, data=basc)
summary(ms_mod2)
confint(ms_mod2)

# Find the Standardized Betas for the coefficients using lmBeta().
lmBeta(ms_mod2) 


# 2. What is the overall proportion of variance accounted for by all four scales?

#Use pCorr() to find the partial and part correlations.
round(pCorr(ms_mod2), 4) 

#######SECONDARY RESEARCH QESTION CONCLUSIONS
# Our primary research question investigated the predictive impact of several Quality of Life 
# items (Physical Health, Psychological, Social Relationships, and Environment) on the outcome of Med 
# School Quality of Life score for Basic Sciences enrolled students. 
# The model showed a   Significant overall effect (F(4, 454) = 50.91 , p<0.05). 
# The four predictor variables accounted for 30.97 percent of the variance in the outcome of Med School Quality of Life. 
# The best predictor of Med School Quality of Life was the Psychological QoL scale (t(454) = 4.452 , p<0.05). 
# As Psychological QoL scale increases one unit, Med School Quality of Life increased by 0.0265 (Standardized beta = 0.2723).  
# Although significant, and the best predictor in the model, Psychological QoL could only uniquely 
# account for 3.0% of the variance in the outcome. 

###RESEARCH QUESTION
# For clinical science students, after controlling for Gender and Age, what predictor has a greater 
# impact on BDI score - State or Trait anxiety?

#Intial Correlations
vars <- c("BDI","Age","Female","State.Anxiety","Trait.anxiety")
cor(clin[,vars], use="pairwise.complete.obs")

# Run a Multiple Linear Regression.
ms_mod3 <- lm(BDI ~ Age + Female + State.Anxiety + Trait.anxiety, data=clin)
summary(ms_mod3)
confint(ms_mod3)

# Find the Standardized Betas for the coefficients using lmBeta().
lmBeta(ms_mod3) 

# 2. What is the overall proportion of variance accounted for by all four scales?
#Use pCorr() to find the partial and part correlations.
round(pCorr(ms_mod3), 4) 