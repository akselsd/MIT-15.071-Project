setwd("~/Desktop/MIT/2019-Fall/15.071/Term Project/Git_Files/MIT-15.071-Project")

# load libraries
source("load_data.R")
library(tidyverse)

# load data
train <- load_data(2009, 2016)
test <- load_data(2017, 2017)
str(train)

##  Linear Regression ############################################

# fit linear regression model
delay.lm <- lm(ARR_DELAY ~. -FL_DATE, data = train)
summary(delay.lm)

# predict
pred1 <- predict(delay.lm, newdata = test)

# model performance
mean_train <- mean(train$ARR_DELAY)

# in-sample
summary(pred1)

# out-of-sample
SSETest <- sum((pred1 - test$ARR_DELAY)^2)
SSTTest <- sum((test$ARR_DELAY - mean_train)^2)
OSR2 <- 1 - SSETest/SSTTest
OSR2

MAE <- sum(abs(test$ARR_DELAY - pred1))/nrow(test)
MAE

RMSE <- sqrt((sum(test$ARR_DELAY - pred1)^2)/nrow(test))
RMSE

# stepwise variable selection
delay.lm.null <- lm(ARR_DELAY~1, data = train)

delay.lm.for <- step(delay.lm.null, scope=list(lower=delay.lm.null, upper=delay.lm), direction = "forward")
summary(delay.lm.for)

flight.lm.back <- step(flight.lm.null, scope=list(lower=flight.lm.null, upper=flight.lm), direction = "backward")

flight.lm.both <- step(flight.lm.null, scope=list(lower=flight.lm.null, upper=flight.lm), direction = "both")

pred1 <- predict(flight.lm.for, data = test)

accuracy(flight.lm, flightTest$ARR_DELAY)


# use step() to run stepwise regression.
# set directions =  to either "backward", "forward", or "both".
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables did it drop?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

## CART ############################################

# load CART libraries

library(rpart) # for building CART model
library(rpart.plot) # a library for an alternative way of plotting CART trees.

# build CART model

delayTree1 = rpart(ARR_DELAY ~. -FL_DATE, data = train)

par(mar=c(1,1,1,1))
prp(delayTree)

# performance of initial tree

CART_train <- predict(delayTree1, newdata = train)
CART_test <- predict(delayTree1, newdata = test)

mean_train <- mean(train$ARR_DELAY)

SSETrain = sum((CART_train - train$ARR_DELAY)^2)
SSTTrain = sum((train$ARR_DELAY - mean_train)^2)

R2 = 1 - SSETrain/SSTTrain
R2

SSETest = sum((CART_test - test$ARR_DELAY)^2)
SSTTest = sum((test$ARR_DELAY - mean_train)^2)
OSR2 = 1 - SSETest/SSTTest
OSR2

MAE = sum(abs(test$ARR_DELAY - CART_test))/nrow(test)
MAE

RMSE = sqrt((sum(test$ARR_DELAY - CART_test)^2)/nrow(test))
RMSE

# cross-validation

set.seed(123)

cv.trees = train(ARR_DELAY~.,
                 data=train,
                 method = "rpart",
                 trControl = trainControl(method = "cv", number = 10), # 10-fold cv
                 tuneGrid = data.frame(.cp = seq(0,.0004,.00001)))   # cp vals: 0, .002, .004, ..., .04; . is part of the syntax

cv.trees

my.best.tree = cv.trees$finalModel
my.best.tree
prp(my.best.tree)

# performance of cross validated tree

delayTree2 = rpart(ARR_DELAY ~ ., data=ames.train, cp=0.0006)

CART_train2 <- predict(delayTree1, newdata = train)
CART_test2 <- predict(delayTree1, newdata = test)

mean_train <- mean(train$ARR_DELAY)

SSETrain = sum((CART_train2 - train$ARR_DELAY)^2)
SSTTrain = sum((train$ARR_DELAY - mean_train)^2)

R2 = 1 - SSETrain/SSTTrain
R2

SSETest = sum((CART_test2 - test$ARR_DELAY)^2)
SSTTest = sum((test$ARR_DELAY - mean_train)^2)
OSR2 = 1 - SSETest/SSTTest
OSR2

MAE = sum(abs(test$ARR_DELAY - CART_test2))/nrow(test)
MAE

RMSE = sqrt((sum(test$ARR_DELAY - CART_test2)^2)/nrow(test))
RMSE