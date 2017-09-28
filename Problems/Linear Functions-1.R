#Primary Research Question
#How has the men's shotput world record changed over time?  What about the women's world record?

library(SDSFoundations)
WR <- WorldRecords

#Create subsets of the dataset that contains only the World Record cases for men's and women's shotput.
menshot<-WR[WR$Event=="Mens Shotput",]
womenshot<-WR[WR$Event=='Womens Shotput',] 

#Create a scatterplot of year and record shotput distance: one for men and one for women.  
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models for Men & Women's data
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)

#Secondary Research Question
#How have the world record times for the men's and the women's mile event changed over the years?

#Create subsets of the data that contains World Record cases for the men's and women's Mile event.
menmile<-WR[WR$Event=="Mens Mile",]
womenmile<-WR[WR$Event=='Womens Mile',]

#Create a scatterplot for each relationship of Mile time and year: one for men and one for women.  
plot(menmile$Year,menmile$Record,main='Mens Mile World Records',xlab='Year',ylab='World Record Time (s)',pch=16)
plot(womenmile$Year,womenmile$Record,main='Womens Mile World Records',xlab='Year',ylab='World Record Time (s)',pch=16)

#Run linear models for Men & Women's data
linFit(menmile$Year,menmile$Record)
linFit(womenmile$Year,womenmile$Record)

#Create a new data frame that contains the world record cases in the men's pole vault event in years 1970 and later.
menspole1970<-subset(WR,WR$Event=="Mens Polevault" & WR$Year>=1970)
#What is the standing world record height (in meters) for men's pole vault?
max(menspole1970$Record)
# Create a scatterplot showing the men's pole vault records since 1970 as a function of year. 
plot(menspole1970$Year,menspole1970$Record)
#Fit a linear model to the data.
linFit(menspole1970$Year,menspole1970$Record)
