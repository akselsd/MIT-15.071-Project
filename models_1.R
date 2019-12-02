setwd("~/Desktop/MIT/2019-Fall/15.071/Term Project/Git_Files/MIT-15.071-Project")

# load source file
source("load_data.R")

# load libraries
library(tidyverse)
library(ROCR)
library(caret) # for randomly splitting training/test 
library(rpart) # for building CART model
library(caTools)
library(dplyr)
library(randomForest)
library(rpart.plot) # a library for an alternative way of plotting CART trees.

# load data
train <- load_aggregate(2009, 2016)
test <- load_aggregate(2017, 2018)
str(train)

##  Linear Regression ############################################

# fit linear regression model
delay.lm <- lm(ARR_DELAY ~., data = train)
summary(delay.lm)

# predict
pred1 <- predict(delay.lm, newdata = test)

# model performance
mean_train <- mean(train$ARR_DELAY)

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

delay.lm.back <- step(delay.lm, scope=list(lower=delay.lm.null, upper=delay.lm), direction = "backward")
summary(delay.lm.back)

delay.lm.both <- step(delay.lm, scope=list(lower=delay.lm.null, upper=delay.lm), direction = "both")

# stepwise prediction
pred2 <- predict(delay.lm.back, newdata = test)

# stepwise performance
SSETest <- sum((pred2 - test$ARR_DELAY)^2)
SSTTest <- sum((test$ARR_DELAY - mean_train)^2)
OSR2 <- 1 - SSETest/SSTTest
OSR2

MAE <- sum(abs(test$ARR_DELAY - pred2))/nrow(test)
MAE

RMSE <- sqrt((sum(test$ARR_DELAY - pred2)^2)/nrow(test))
RMSE

## CART ############################################

# build CART model

delayTree1 = rpart(ARR_DELAY ~., data = train, cp=1.5e-05)

par(mar=c(1,1,1,1))
prp(delayTree1)

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
                 tuneGrid = data.frame(.cp = seq(0,1e-04,1e-06)))   # cp vals: 0, .002, .004, ..., .04; . is part of the syntax

cv.trees

my.best.tree = cv.trees$finalModel
my.best.tree
prp(my.best.tree)

# performance of cross validated tree

delayTree2 = rpart(ARR_DELAY ~ ., data=train, cp=1.5e-05)

CART_train2 <- predict(delayTree2, newdata = train)
CART_test2 <- predict(delayTree2, newdata = test)

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

## Random Forest ############################################

rf.mod1 = randomForest(ARR_DELAY~., data=train)  # Default:  ntree = 500, mtry = ncols/3 = 10/3, nodesize = 5
# Specify different parameters to make it run faster:
set.seed(144)
rf.mod2 = randomForest(ARR_DELAY~., data=train, mtry=2, nodesize=20, ntree=500)
# mtry: number of variables to choose from at each split
# nodesize: equivalent to CART minbucket
# ntrees: number of trees

# While you cannot visualize a forest well, you can get some sense of the relative importance of variables
# used in the model by ranking how often each variable appears in trees in the forest.
# You can use the importance() function to list the relative importance of variables
# Notice these values are NOT sorted: 
important_vars = importance(rf.mod1)
important_vars
# This importance measure is the average increase in node purity when a node splits on that variable.
# For regression trees, node purity is measured by residual sum of squares. For details, ?importance. 
# It provides some high-level insights on the main drivers of the outcome (click-through rates, here)

# Predictions are the same as usual.
pred.test = predict(rf.mod1, newdata=test)

# Assessment of out-of-sample performance
R2.rfFINAL <- 1-sum((pred.test - test$ARR_DELAY)^2)/sum((mean(train$ARR_DELAY) - test$ARR_DELAY)^2)
mae.rfFINAL <- mean(abs(pred.test - test$ARR_DELAY)
rmse.rfFINAL <- sqrt(mean((pred.test - test$ARR_DELAY)^2))

#########################################
## Cross Validation for Random Forests ##
#########################################

# Just like we cross-validated individual trees in the last recitation,
# We can cross-validate the entire random forest over some parameter
# Here we cross-validate over mtry, the number of variables to consider in each tree
# You can run it if you would like, but know it will take some time.
# If you choose not to run it now, make sure you understand how the command works
# it is very similar to our code for CART.

##### Cross Validation ####
set.seed(144)
# This code would usually take a LONG time to run. It uses the default values of the parameters, including ntree=500
# rf.cv.full = train(y = ctrTrain$CTR,
#               x = subset(ctrTrain, select=-c(CTR)),
#               method="rf",
#               trControl=trainControl(method="cv", number=10),
#               tuneGrid=data.frame(mtry=seq(1,10,1)))

# Instead, we are using ntree = 10 so that the code can run quickly during recitation.
rf.cv = train(y = train$ARR_DELAY,
              x = subset(train, select=-c(ARR_DELAY)),
              method="rf", nodesize=20, ntree=10,
              trControl=trainControl(method="cv", number=10),  # 10-fold 
              tuneGrid=data.frame(mtry=seq(1,10,1)))           # tune mtry

# The cross validation results show us that the best model uses mtry = 3 if we use RMSE as our criterion.
rf.cv
# We can plot the relationship between mtry and RMSE
plot(rf.cv$results$mtry, rf.cv$results$RMSE, type = "l",xlab = "mtry",ylab = "RMSE")
axis(side = 1, at = 1:10)   # customize x ticks
# Like before, we can extract the best model using:
rf.mod.final = rf.cv$finalModel
# When we extract the final model, note that R re-trained it in the back end with the full training set.

# Finally, we can make predictions
pred.test = predict(rf.mod.final, newdata=test)

# Calculate OSR2 as usual.
# The number may not be very good because we only used 10 trees in our RF.
SSETest = sum((pred.test - test$ARR_DELAY)^2)
SSTTest = sum((test$ARR_DELAY - mean(train$ARR_DELAY))^2)
OSR2 = 1 - SSETest/SSTTest
OSR2
