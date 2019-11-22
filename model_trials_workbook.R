#trial of load data
source("load_data.R")
source("utils.R")


train <- load_data(2009,2016)
test <- load_data(2017,2018)
summary(train)
summary(test)



tesmodel = glm( ISDELAYED~ + TIME_OF_DAY + AIR_TIME + DISTANCE, data=train, family=binomial)
summary(model)

pred =predict.glm(tesmodel,test)
threshold <- pred > .1
table(threshold)
table(test$ISDELAYED)
confusion.matrix = table(test$ISDELAYED,threshold)
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy

summary(pred)


findfactors = lm(train$ARR_DELAY ~ ., data = train)
