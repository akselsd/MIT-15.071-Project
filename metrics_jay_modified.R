setwd("~/Desktop/MIT/2019-Fall/15.071/Term Project/Git_Files/MIT-15.071-Project")

# load libraries
source("load_data.R")
library(ggplot2)
library(tidyverse)

# load data
train = load_data(2009, 2016)
test = load_data(2017, 2018)

summary(flight)
str(flight)

#Only flights originating in Boston
Departing = subset(train, ORIGIN == "BOS")
Arriving = subset(train, DEST == "BOS")

AvgDelaybyCarrierDEP = aggregate(Departing$ARR_DELAY, by = list(Departing$OP_CARRIER), FUN = mean)

plot(Departing$OP_CARRIER,AvgDelaybyCarrierDEP)
plot(AvgDelaybyCarrierDEP)

qplot(train$distance,train$CRS_ELAPSED_TIME,color=train$ARR_DELAY)
ggplot(train,aes(CRS_ELAPSED_TIME))+ geom_point()
AvgDelaybyCarrierARR = aggregate(Arriving$ARR_DELAY, by = list(Arriving$OP_CARRIER), FUN = mean)

plot(Arriving$OP_CARRIER,AvgDelaybyCarrierARR, AvgDelaybyCarrierDEP)
plot(AvgDelaybyCarrierARR)



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

