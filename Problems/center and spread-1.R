#data to vector
overtime_hours<-c(10,   2,   6,   12,   14,   15,   15,   24,   15,   25,   3,   12)

#The data mean
mean(overtime_hours)

#The data standard deviation
sd(overtime_hours)

#histogram -By default, the "Sturges" algorithm is used to automatically determine the number of bins.
hist(overtime_hours,breaks="Sturges")

#The interquartile range
IQR(overtime_hours)

#The quartiles -NOT SURE WHICH WAY IS RIGHT - SEE BELOW
quantile(overtime_hours)

#Another way to find quartiles - NOT SURE WHICH WAY IS RIGHT
fivenum(overtime_hours)

# one more other ways to calculate quartiles - NOT SURE WHICH WAY IS CORRECT
summary(overtime_hours)

# The range of data
max(overtime_hours) ??? min(overtime_hours) 

# The median value of data
median(overtime_hours)
