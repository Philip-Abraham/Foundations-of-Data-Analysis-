# Some nerve cells have the ability to regenerate. Researchers think that these cells may generate creatine phosphate (CP) to stimulate new cell growth.
# To test this hypothesis, researchers cut the nerves emanating from the left side of the spinal cord in a 
# sample of rhesus monkeys, while the nerves on the right side were kept intact.  They then compared the CP levels (mg/100g) in nerve cells on both sides. 

setwd("C:/Users/Philip Abraham/OneDrive/TECHNOLOGY/Data Analysis/Data")
nerve_reg <- read.csv(file="nerve_cells_regen.csv",head=TRUE,sep=",")

# Assume the researchers calculated the difference scores as diff = CPleft - CPright.
# They set alpha= 0.05. 

#Difference Dataframe subset
diff<-nerve_reg$diff
# Number of Samples and degrees of freedom (df)
n<-length(diff)
df<-n-1

# What is the t-critical value? 
alpha<-0.05
qt(alpha,df,lower.tail = F)

#How much of a difference in creatine phosphate was observed, on average, between the left and right nerve cells?
avg<-mean(diff)
#What is the Standard Deviation of the difference scores?
stddev<-sd(diff)
#What is the Standard Error for your t-test?
SE<-stddev/sqrt(n)

# What is your test statistic?
t.test(nerve_reg[,1],nerve_reg[,2],paired = T)
