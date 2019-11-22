setwd("~/Desktop/MIT/2019-Fall/15.071/Term Project/Git_Files/MIT-15.071-Project")

# load libraries
source("load_data.R")
library(tidyverse)

# load data
flightTrain <- load_data(2013, 2016)
flightTest <- load_data(2017, 2017)

##  Linear Regression ############################################

# linear regression model
flight.lm <- lm(ARR_DELAY ~., data = flightTrain)
summary(mod1)

pred1 <- predict(flight.lm)


PredictTrain2 = predict(amesTree2, newdata = ames.train)
PredictTest2 = predict(amesTree2, newdata = ames.test)

mean_train = mean(ames.train$SalePrice)
mean_train

SSETrain = sum((PredictTrain2 - ames.train$SalePrice)^2)
SSTTrain = sum((ames.train$SalePrice - mean_train)^2)

R2 = 1 - SSETrain/SSTTrain
R2

SSETest = sum((PredictTest2 - ames.test$SalePrice)^2)
SSTTest = sum((ames.test$SalePrice - mean_train)^2)
OSR2 = 1 - SSETest/SSTTest
OSR2

MAE = sum(abs(ames.test$SalePrice-PredictTest2))/nrow(ames.test)
MAE

RMSE = sqrt((sum(ames.test$SalePrice-PredictTest2)^2)/nrow(ames.test))
RMSE



# stepwise variable selection
flight.lm.null <- lm(ARR_DELAY~1, data = flightTrain)

flight.lm.for <- step(flight.lm.null, scope=list(lower=flight.lm.null, upper=flight.lm), direction = "forward")

flight.lm.back <- step(flight.lm.null, scope=list(lower=flight.lm.null, upper=flight.lm), direction = "backward")

flight.lm.both <- step(flight.lm.null, scope=list(lower=flight.lm.null, upper=flight.lm), direction = "backward")

pred1 <- predict(flight.lm, data = flightTest)

accuracy(flight.lm, flightTest$ARR_DELAY)


# use step() to run stepwise regression.
# set directions =  to either "backward", "forward", or "both".
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables did it drop?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

## CART ############################################

# CART

library(rpart) # for building CART model
library(rpart.plot) # a library for an alternative way of plotting CART trees.

flightTree = rpart(flight ~ ., data=flight_train)

par(mar=c(1,1,1,1))
prp(claimsTree)

set.seed(123)

# Create a user-defined function (returns a 2-element array)
Loss <- function(data, lev = NULL, model = NULL, ...) {
  c(AvgLoss = mean(data$weights * (data$obs != data$pred)), Accuracy = mean(data$obs == data$pred))
}

# CV on cp vals
cv.trees = train(Violator~Male+Age+TimeServed+Class+Multiple+InCity,  # *explicitly* enumerate the indep. vars
                 data=parole.train,
                 method = "rpart",
                 weights = ifelse(parole.train$Violator == 1, 20, 1), # loss function
                 trControl = trainControl(method = "cv", number = 10, summaryFunction=Loss), # 10-fold cv
                 metric="AvgLoss", maximize=FALSE,                    # minimize the AvgLoss 
                 tuneGrid = data.frame(.cp = seq(0, .04, by=.002)))   # cp vals: 0, .002, .004, ..., .04; . is part of the syntax

# PART B: Constructing CART Model  #########################

amesTree1 = rpart(SalePrice ~ ., data=ames.train)
amesTree1

# To plot the tree, we can use use prp (for "plot rpart")
par(mar=c(1,1,1,1))
prp(amesTree1)

tapply(ames, "CentralAir", mean)

mean(ames.train$SalePrice)

NoCA <- subset(ames,CentralAir=="N")
mean(NoCA$SalePrice)
YesCA <- subset(ames,CentralAir=="Y")
mean(YesCA$SalePrice)


# Performance of initial Tree  #########################

PredictTrain = predict(amesTree1, newdata = ames.train)
PredictTest = predict(amesTree1, newdata = ames.test)

mean_train = mean(ames.train$SalePrice)
mean_train

SSETrain = sum((PredictTrain - ames.train$SalePrice)^2)
SSTTrain = sum((ames.train$SalePrice - mean_train)^2)

R2 = 1 - SSETrain/SSTTrain
R2

SSETest = sum((PredictTest - ames.test$SalePrice)^2)
SSTTest = sum((ames.test$SalePrice - mean_train)^2)
OSR2 = 1 - SSETest/SSTTest
OSR2

MAE = sum(abs(ames.test$SalePrice-PredictTest))/nrow(ames.test)
MAE

RMSE = sqrt((sum(ames.test$SalePrice-PredictTest)^2)/nrow(ames.test))
RMSE

# PART D: Cross-Validation  #########################

cv.trees = train(SalePrice~.,
                 data=ames.train,
                 method = "rpart",
                 trControl = trainControl(method = "cv", number = 10), # 10-fold cv
                 tuneGrid = data.frame(.cp = seq(0,.0004,.00001)))   # cp vals: 0, .002, .004, ..., .04; . is part of the syntax

cv.trees

my.best.tree = cv.trees$finalModel
my.best.tree
prp(my.best.tree)
?prp

# PART D: Performance of New Tree  #########################

amesTree2 = rpart(SalePrice ~ ., data=ames.train, cp=0.0006)

PredictTrain2 = predict(amesTree2, newdata = ames.train)
PredictTest2 = predict(amesTree2, newdata = ames.test)

mean_train = mean(ames.train$SalePrice)
mean_train

SSETrain = sum((PredictTrain2 - ames.train$SalePrice)^2)
SSTTrain = sum((ames.train$SalePrice - mean_train)^2)

R2 = 1 - SSETrain/SSTTrain
R2

SSETest = sum((PredictTest2 - ames.test$SalePrice)^2)
SSTTest = sum((ames.test$SalePrice - mean_train)^2)
OSR2 = 1 - SSETest/SSTTest
OSR2

MAE = sum(abs(ames.test$SalePrice-PredictTest2))/nrow(ames.test)
MAE

RMSE = sqrt((sum(ames.test$SalePrice-PredictTest2)^2)/nrow(ames.test))
RMSE


