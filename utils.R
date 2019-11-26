library(tidyverse)

# Remove carrier
# test = test[test$OP_CARRIER != "AA",]

add_timeofday_column <- function(df) {
  df$TIME_OF_DAY=cut(df$CRS_DEP_TIME, c(0,600,1200,1800,2400))
  levels(df$TIME_OF_DAY) = c("night","morning","afternoon","evening")
  return(df)
}

add_hours_column <- function(df) {
  df$TIME_OF_DAY= cut(df$CRS_DEP_TIME, c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400))
  levels(df$TIME_OF_DAY) = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
  return(df)
}



add_seasonal_data <- function(df)
{
  date <- as.Date(df$FL_DATE)
  monthnum = months(date)
  
  df$SEASON = fct_collapse(monthnum,
                           winter = c("December", "January", "February"),
                           spring = c("March", "April", "May"),
                           summer  = c("June", "July", "August"),
                           fall = c("September", "October", "November"))
  
  return(df)
}

add_binomial <- function(df,t)
{
  df$ISDELAYED = as.numeric(df$ARR_DELAY > t)
  return(df)
}