### z-Statistic
#Conducting a Hypothesis Test on One Sample Mean (x) with sample size (n) When the Population Parameters (u and stddev) are 
#Known and a right-tail significance level (a) is given.

hyp_z<-function(u,stddev,x,n,a){
        
        z_table<-qnorm(1-a)
        
        SE<-stddev/sqrt(n)
        z_calc<-(x-u)/SE
        
        if(z_calc<z_table){
                print("FAIL TO REJECT THE NULL HYPOTHESIS")
        }else{
                print("REJECT NULL HYPOTHESIS")
        }
        
}
### t-Statistic - t-Test for One Sample Mean - non-directional hypothesis test
#Conducting a Hypothesis Test on One Sample Mean (x) with sample size (n) When the Population std dev is not known
#not known but, the sample stddev and population mean (u) under the null hypothesis and the alpha level (a) is given.

hyp_t<-function(u,x,stddev,n,a){
        
        df<-n-1
         
        SE<-stddev/sqrt(n)
        t_calc<-(x-u)/SE
        
        p_value<-2*pt(t_calc,df)
        
        if(p_value>a){
                print("FAIL TO REJECT THE NULL HYPOTHESIS")
        }else{
                print("REJECT NULL HYPOTHESIS")
        }
        
}

####### UPPER TAILED t-TEST
# An industrial plant dumps its waste into a nearby river, but claims that it is not impacting the native
# species of frogs that live in the river.  The frogs are able to tolerate calcium concentrations up to 
# 91 mg/L. You measure the concentration of calcium in 25 random samples from the river.  Your 
# measurements are approximately normally distributed, with a mean of 93.6 mg/L, with a standard 
# deviation of 7.8 mg/L.  
# The appropriate alternative hypothesis if the industrial plant's runoff is believed to be producing 
# higher calcium concentrations than are deemed acceptable for the frogs is u > 91.

#  Calculate the test statistic.
sample.mean <- 93.6       
population.mean <-91
standard.deviation <- 7.8
n <- 25 # number of random samples
alpha <- 0.05 #alpha level

t <- (sample.mean - population.mean) / (standard.deviation / sqrt(n)) 
print(paste("t statistic:", t))

#Now to create the corresponding plot we draw a random sample between -4 and 4 and assign the corresponding sequence to hx, we also compute the critical value with qt().
minx <- -4
maxx <- 4

x <- seq(minx, maxx, length = 100)

# The function dt() is used to generate random numbers whose probability distribution is the corresponding 
# Student's t with n-1 degrees of freedom. Those samples are latter plotted with plot(), highlighting the
# rejection zone and the critical value.

hx <- dt(x, df = n-1)

# What is the t-critical value?
critical.value<-qt(1-alpha, df = n-1)

#Set up the plot, highlighting the critical region in purple
plot(x, hx, type="n", 
     xlab = "Critical Region", ylab = "",           # axes labels
     main = paste("Student's t Distribution\n",     # title
                  n-1, "degrees of freedom"), 
     axes = FALSE)                                  # switch axes off

lines(x, hx)                # plots the Student's t distribution

i <- x >= critical.value  # points in the rejection zone 
polygon(c(critical.value, x[i], maxx), c(0, hx[i], 0), col = "purple")

axis(1, pos = 0,              # tick marks
     at = c(t, critical.value, minx, 0, maxx), 
     labels = format(c(t, 
                       critical.value, 
                       minx, 0, maxx), digits=2)
)

####### TWO TAILED t-TEST
# You are studying a population of peregrine falcons and want to estimate their average wingspan.  
# So you collect a random sample of 12 adult male birds and measure a mean wingspan of 42.6 cm, with a 
# standard deviation of 5.3 cm. 
# Assume that the distribution of measurements was approximately normal.

## To compute t the formula to use is: 
## t = (sample mean - population mean)/(sample standard deviation / square root of the sample size))
sample.mean <- 42.6        
population.mean <- 42.6
standard.deviation <- 5.3
n <- 12
df<-n-1
alpha <- 0.1

t <- (sample.mean - population.mean) / (standard.deviation / sqrt(n)) 
print(paste("t statistic:", t))

#Now to create the corresponding plot we draw a random sample between -4 and 4 and assign the corresponding sequence to hx, we also compute the critical value with qt().
minx <- -4
maxx <- 4

x <- seq(minx, maxx, length = 100)

# The function dt() is used to generate random numbers whose probability distribution is the corresponding 
# Student's t with n-1 degrees of freedom. Those samples are latter plotted with plot(), highlighting the
# rejection zone and the critical value.
hx <- dt(x, df = n-1)

crtclvalue.right = qt(1-alpha/2, df = n-1)
crtclvalue.left = -crtclvalue.right

#Set up the plot, highlighting the critical region in purple
plot(x, hx, type="n", 
     xlab = "Critical Region", ylab = "",           # axes labels
     main = paste("Student's t Distribution\n",     # title
                  n-1, "degrees of freedom"), 
     axes = FALSE)                                  # switch axes off

lines(x, hx)                # plots the Student's t distribution

i <- x >= crtclvalue.right  # points in the right rejection zone 
polygon(c(crtclvalue.right, x[i], maxx), c(0, hx[i], 0), col = "purple")

j <- x <= crtclvalue.left   # points in the left rejection zone
polygon(c(minx, x[j], crtclvalue.left), c(0, hx[j], 0), col = "purple") 

axis(1, pos = 0,              # tick marks
     at = c(t, crtclvalue.left, crtclvalue.right, minx, 0, maxx), 
     labels = format(c(t, 
                       crtclvalue.left, 
                       crtclvalue.right, 
                       minx, 0, maxx), digits=2)
)

##Calculate a 90% confidence interval for the mean wingspan for the population of male peregrine falcons.
alpha_given<-.1
t_crit<-abs(qt(alpha_given/2,df))
x_Upper_Bound<-sample.mean +((t_crit)*(standard.deviation)/sqrt(n))
x_Lower_Bound<-sample.mean -((t_crit)*(standard.deviation)/sqrt(n))

##Calculate a 95% confidence interval for the mean wingspan for the population of male peregrine falcons.
alpha_given<-.05
t_crit<-abs(qt(alpha_given/2,df))
x_Upper_Bound<-sample.mean +((t_crit)*(standard.deviation)/sqrt(n))
x_Lower_Bound<-sample.mean -((t_crit)*(standard.deviation)/sqrt(n))

#######################################################
#The t-table is symetrical
qt(c(.05,0.95),15)
