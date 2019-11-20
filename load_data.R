#
# Example usage: 
#
# source("load_data.R")           # Import this function
# train = load_data(2013, 2017)   # Training data from 2013 to 2017
# test = load_data(2018, 2018)    # Testing data, 2018 only
#
# Needs package testit, to install run install.packages("testit")
#
library(testit)
source("utils.R")

load_data <- function(startyear, endyear) {
  df = load_data_cancelled(startyear, endyear)
  df = df[df$CANCELLED == 0,]
  
  drops = c("CANCELLED", "CANCELLATION_CODE")
  
  df = df[, !(names(df) %in% drops)]
  
  assert("No NA in arrival delay column", nrow(df[is.na(df$ARR_DELAY),]) == 0)
  assert("No NA in departure delay column", nrow(df[is.na(df$DEP_DELAY),]) == 0)
  assert("No cancelled flights", nrow(df[df$CANCELLED == 1,]) == 0)
  assert("No diverted flights", nrow(df[df$DIVERTED == 1,]) == 0)
  
  return(df)
}


#the function below includes cancelled flights in the data set and zeros them instead of NA
#and is used in the main function load_data

load_data_cancelled <- function(startyear, endyear) {
  
  assert("Endyear is greater than or equal to startyear", endyear >= startyear)
  df = data.frame()
  
  for (year in seq(startyear, endyear)) {
    infile = paste("Data/", toString(startyear), "_boston.csv", sep="")
    data = read.csv(infile)
    df = rbind(df, data)
  }
  
  df = df[df$DIVERTED == 0,]
  
  drops = c("DIVERTED", "Unnamed..27", "X")
  
  df = df[, !(names(df) %in% drops)]
  
  df$ARR_DELAY[df$ARR_TIME == df$CRS_ARR_TIME] <- 0
  df$DEP_DELAY[df$DEP_TIME == df$CRS_DEP_TIME] <- 0
  
  df = add_timeofday_column(df)
  
  return(df)
}