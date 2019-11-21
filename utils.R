library(tidyverse)

# Remove carrier
# test = test[test$OP_CARRIER != "AA",]

add_timeofday_column <- function(df) {
  df$TIME_OF_DAY=cut(df$CRS_DEP_TIME, c(0,600,1200,1800,2400))
  levels(df$TIME_OF_DAY) = c("night","morning","afternoon","evening")
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