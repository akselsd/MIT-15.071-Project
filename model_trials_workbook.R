#trial of load data
source("load_data.R")
source("utils.R")


train <- load_data(2009,2016)
test <- load_data(2017,2018)
summary(train)
summary(test)


#LOGISTIC MODEL TRIAL WITH NO CONFLICTING VARIABLES BETWEEN TEST AND TRIAL
tesmodel = glm( ISDELAYED~  SEASON + TIME_OF_DAY + AIR_TIME + DISTANCE, data=train, family=binomial)
summary(model)

pred =predict.glm(tesmodel,test)
threshold <- pred > .1
table(threshold)
table(test$ISDELAYED)
confusion.matrix = table(test$ISDELAYED,threshold)
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy


#data types that can be accessed in reality

#FL_DATE + SEASON + Weekday + TIME_OF_DAY + ORIGIN + DEST + OP_CARRIER + OP_CARRIER_FL_NUM + CRS_DEP_TIME + CRS_ARR_TIME + CRS_ELAPSED_TIME + DISTANCE)

#subsetting data types for 
train_future <- subset(train, select = c(FL_DATE, OP_CARRIER, OP_CARRIER_FL_NUM, CRS_DEP_TIME, CRS_ARR_TIME , CRS_ELAPSED_TIME, DISTANCE, SEASON , Weekday , TIME_OF_DAY , ORIGIN , DEST, ISDELAYED))
test_future <- subset(test, select = c(FL_DATE, OP_CARRIER, OP_CARRIER_FL_NUM, CRS_DEP_TIME, CRS_ARR_TIME , CRS_ELAPSED_TIME, DISTANCE, SEASON , Weekday , TIME_OF_DAY , ORIGIN , DEST, ISDELAYED))

summary(train_future) 
 summary(pred)


findfactors = lm(ARR_DELAY ~ . - FL_DATE, data = train)
summary(findfactors)

tesmodel = glm( ISDELAYED~ ., data=train_future, family=binomial)
summary(model)

pred =predict.glm(tesmodel,test_future)
threshold <- pred > .1
table(threshold)
table(test_future$ISDELAYED)
confusion.matrix = table(test_future$ISDELAYED,threshold)
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy
