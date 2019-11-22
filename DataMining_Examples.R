###########################################################################################
### Chapter 8
#### Table 8.4

library(e1071)
delays.df <- read.csv("FlightDelays.csv")

# change numerical variables to categorical first
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
delays.df$DEP_TIME <- factor(delays.df$DEP_TIME)
# create hourly bins departure time 
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# Create training and validation sets.
selected.var <- c(10, 1, 8, 4, 2, 13)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run naive bayes
delays.nb <- naiveBayes(Flight.Status ~ ., data = train.df)
delays.nb


#### Table 8.5

# use prop.table() with margin = 1 to convert a count table to a proportion table, 
# where each row sums up to 1 (use margin = 2 for column sums).
prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 1)

#### Table 8.6

## predict probabilities
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
## predict class membership
pred.class <- predict(delays.nb, newdata = valid.df)

df <- data.frame(actual = valid.df$Flight.Status, predicted = pred.class, pred.prob)

df[valid.df$CARRIER == "DL" & valid.df$DAY_WEEK == 7 & valid.df$CRS_DEP_TIME == 10 & 
     valid.df$DEST == "LGA" & valid.df$ORIGIN == "DCA",]

#### Table 8.7

library(caret)

# training
pred.class <- predict(delays.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$Flight.Status)

# validation
pred.class <- predict(delays.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)

#### Figure 8.1

library(gains)
gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[,1], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Flight.Status=="delayed"))~c(0, dim(valid.df)[1]), lty=2)

###########################################################################################

#### Figure 10.4

# code for generating top-right bar chart
# for other plots, replace aggregating variable by setting argument by =  in 
# aggregate(). 
# in function barplot(), set the x-label (argument xlab =) and y-label 
# (argument names.arg =) 
# according to the variable of choice.  

delays.df <- read.csv("FlightDelays.csv")

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DAY_WEEK), 
                  mean, rm.na = T)[,2], xlab = "Day of Week", ylab = "Average Delay", 
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


#### Table 10.6

delays.df <- read.csv("FlightDelays.csv")

# transform variables and create bins
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK, levels = c(1:7), 
                             labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# create reference categories
delays.df$ORIGIN <- relevel(delays.df$ORIGIN, ref = "IAD")
delays.df$DEST <- relevel(delays.df$DEST, ref = "LGA")
delays.df$CARRIER <- relevel(delays.df$CARRIER, ref = "US")
delays.df$DAY_WEEK <- relevel(delays.df$DAY_WEEK, ref = "Wed")
delays.df$isDelay <- 1 * (delays.df$Flight.Status == "delayed")

# create training and validation sets
selected.var <- c(10, 1, 8, 4, 2, 9, 14)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run logistic model, and show coefficients and odds
lm.fit <- glm(isDelay ~ ., data = train.df, family = "binomial")
data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit))) 

round(data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit))), 5)


#### Figure 10.6

library(gains)
pred <- predict(lm.fit, valid.df)
gain <- gains(valid.df$isDelay, pred, groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$isDelay))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$isDelay))~c(0, dim(valid.df)[1]), lty=2)


#### Table 10.8

# fewer predictors
delays.df$Weekend <- delays.df$DAY_WEEK %in% c("Sun", "Sat")
delays.df$CARRIER_CO_MQ_DH_RU <- delays.df$CARRIER %in% c("CO", "MQ", "DH", "RU")
delays.df$MORNING <- delays.df$CRS_DEP_TIME %in% c(6, 7, 8, 9)
delays.df$NOON <- delays.df$CRS_DEP_TIME %in% c(10, 11, 12, 13)
delays.df$AFTER2P <- delays.df$CRS_DEP_TIME %in% c(14, 15, 16, 17, 18)
delays.df$EVENING <- delays.df$CRS_DEP_TIME %in% c(19, 20)


set.seed(1)  # Set the seed for the random number generator for reproducing the 
# partition.
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(delays.df)[1]), train.index)  
train.df <- delays.df[train.index, ]
valid.df <- delays.df[valid.index, ]

lm.fit <- glm(isDelay ~ Weekend + Weather + CARRIER_CO_MQ_DH_RU + MORNING  +  NOON + 
                AFTER2P + EVENING, data = train.df, family = "binomial")
summary(lm.fit)

# evaluate
pred <- predict(lm.fit, valid.df)
confusionMatrix(factor(1*(pred > 0.5)), factor(valid.df$isDelay))


