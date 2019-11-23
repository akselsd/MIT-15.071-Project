setwd("~/Desktop/MIT/2019-Fall/15.071/Term Project/Git_Files/MIT-15.071-Project")

# load libraries
source("load_data.R")
library(ggplot2)
library(tidyverse)

# load data
train = load_data(2009, 2015)
test = load_data(2016, 2018)

summary(flight)
str(flight)

#Only flights originating in Boston
BOS = subset(flight, ORIGIN == "BOS")

#Flights by Weekday
table(BOS$WEEKDAY)

#Avg delay by Weekday
avg.weekday.delay = aggregate(BOS$ARR_DELAY, by=list(BOS$WEEKDAY), FUN=mean)
avg.weekday.delay

plot(avg.weekday.delay)
plot(BOS$WEEKDAY, BOS$ARR_DELAY, xlab = "Weekday", ylab = "Delay Time")

barplot(aggregate(BOS$Flight.Status == "delayed", by = list(BOS$WEEKDAY), 
                  mean, rm.na = T)[,2], xlab = "Day of Week", ylab = "Average Delay", 
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


#Delay by Carrier
DelayByCarrier = aggregate(BOS$ARR_DELAY, by=list(BOS$OP_CARRIER), FUN=mean)
DelayByCarrier

plot(BOS$OP_CARRIER, DelayByCarrier)
plot(BOS$OP_CARRIER, DelayByCarrier, xlab = "Carrier", ylab = "Delay Time")

