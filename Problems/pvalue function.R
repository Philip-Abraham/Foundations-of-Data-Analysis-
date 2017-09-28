###P-Value is the probability of obtaining the value of the test statistic we did if the null hypothesis is true


# write a function that takes the population mean (U), population standard deviation (s), sample mean (x), 
#  and sample size (n) and generates the p-value.

#right-tailed test
pvalue<-function(u,s,x,n){
        SE<-s/sqrt(n)
        return(1-pnorm(x,u,SE))
}

#two-tailed test
pvalue<-function(u,s,xH,xL,n){
        SE<-s/sqrt(n)
        return(2*(1-pnorm(xH,u,SE)))
}
