setwd("~/Desktop/MIT/2019-Fall/15.071/Term Project/Git_Files/MIT-15.071-Project")

source("load_data.R")
library(ggplot2)
library(tidyverse)

## === Delay types ===
flight = load_data(2018, 2018)

summary(flight)
str(flight)


#Only flights originating in Boston
BOS = subset(flight, ORIGIN == "BOS")

#Add Weekday column
BOS$Weekday = weekdays(as.Date(BOS$FL_DATE,'%Y-%m-%d'))

#Flights by Weekday
table(BOS$Weekday)

#Avg delay by Weekday
avg.weekday.delay = aggregate(BOS$ARR_DELAY, by=list(BOS$Weekday), FUN=mean)
avg.weekday.delay

days = list(BOS$Weekday)
days

plot(BOS$Weekday, avg.weekday.delay)
plot(BOS$Weekday, BOS$ARR_DELAY, xlab = "Weekday", ylab = "Delay Time")



delays.df <- read.csv("FlightDelays.csv")

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DAY_WEEK), 
                  mean, rm.na = T)[,2], xlab = "Day of Week", ylab = "Average Delay", 
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


#Delay by Carrier
DelayByCarrier = aggregate(BOS$ARR_DELAY, by=list(BOS$OP_CARRIER), FUN=mean)
DelayByCarrier

plot(BOS$OP_CARRIER, DelayByCarrier)
plot(BOS$OP_CARRIER, DelayByCarrier, xlab = "Carrier", ylab = "Delay Time")

#Canceled flights
BOS_cancelled = subset(flight18, CANCELLED == 1)
BOS_blank = BOS18[is.na(FL_DATE)]

nrow(BOS18$DIVERTED == 1)

plot(BOS18)

table(BOS18$OP_CARRIER, BOS18$)

str(BOS18)

# Model Testing

pred









flight <- read.csv("2018.csv")

str(flight)
head(flight)

flight <- read.csv("2018.csv")
boston_18 <- subset(flight, ORIGIN=="BOS" | DEST=="BOS")
write.csv(boston_18, file="Boston_2018.csv")

flight <- read.csv("2016.csv")
boston_16 <- subset(flight, ORIGIN=="BOS" | DEST=="BOS")
write.csv(boston_16, file="Boston_2016.csv")



?write.csv
?subset
  
#What if you want to find out how many missing values ("na") are present in your data frame?
is.na(flight) #returns a large matrix of Booleans (TRUE/FALSE)
#We want to count the total, so we need to use sum(). It's convention that TRUE = 1, FALSE = 0.
sum(is.na(flight))

#What if we want to know which entries had "na"? Use which(), and pass the additional argument arr.ind = TRUE so that it keeps track of the rows and columns of the matrix
which(is.na(flight), arr.ind = T) #We feed which() an array of TRUE and FALSE elements, and it returns the index of the TRUE ones

#What if we wanted to find the number of elements that are NOT "na"? Easy, just use the "not" operator !
sum(!is.na(flight)) # ! converts the TRUE/FALSE values that follows it to their opposite values

#percent of missing data
percent.missing <- sum(is.na(flight))/(sum(!is.na(flight))+sum(is.na(flight)))
percent.missing

flightTrain <- flight[flight$Year <= 1985,] # the comma is important! [It means "select all columns"]
flightTest <- wine[wine$Year > 1985,]
# Now, we have two separate data frames:
# 1) wineTrain - will be used to build the regression models.
# 2) wineTest - will be used to evaluate the OSR^2, MAE, RMSE of our models.
# Take a look at these data frames in the Environment window
# to make sure everything looks right.

# We are now ready to build our initial regression model
# Regression models are constructed with 'lm' (linear model)
# There are two parts: the 'equation' and the data we are using
mod1 <- lm(LogAuctionIndex ~ WinterRain + HarvestRain + GrowTemp + HarvestTemp + Age + FrancePop + USAlcConsump, data = flight)

bos_18 <- read.csv("2018_boston.csv")

ggplot(bos_18, aes(x=MoSold)) + geom_histogram(color="blue", fill="blue", binwidth=1) 
ggplot(ames, aes(fill=MSZoning, y=SalePrice, x=MoSold)) + geom_bar(position="stack", stat="identity") plot(ames$YearBuilt, ames$SalePrice, xlab = "Year Built", ylab = "Sale Price") 

plot(ames$YearBuilt, ames$SalePrice, xlab = "Year Built", ylab = "Sale Price") 

source("load_data.R")
flights = load_data(2009,2018)


plot(flights$DEP_DELAY, flights$FL_DATE)

source("load_data.R")
flights_09 <- load_data(2009,2009)
plot(flights_09$DEP_DELAY, flights_09$FL_DATE)

Bos_Dep_09 <- subset(flights_09, ORIGIN=="BOS")
plot(Bos_Dep_09$FL_DATE, Bos_Dep_09$DEP_DELAY, xlab = "Date", ylab = "Delay Time")


boston_18 <- subset(flight, ORIGIN=="BOS" | DEST=="BOS")
