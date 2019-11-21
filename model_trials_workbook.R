#trial of load data
source("load_data.R")
source("utils.R")


train <- load_data(2009,2017)
summary(train)



model = glm( ISDELAYED~ OP_CARRIER + ORIGIN + DEST + TIME_OF_DAY + season + AIR_TIME + DISTANCE, data=train2, family=binomial)
summary(model)

test = load_data(2010,2010)
test = seasonal_data(test)
test = add_timeofday_column(test)
test = add_binomial(test,15)
pred =predict.glm(model,test)
