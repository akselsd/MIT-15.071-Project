

add_timeofday_column <- function(df) {
  df$TIME_OF_DAY=cut(df$CRS_DEP_TIME, c(0,600,1200,1800,2400))
  levels(df$TIME_OF_DAY) = c("night","morning","afternoon","evening")
  return(df)
}



add_seasonal_data <- function(df)
{
  date <- as.Date(df$FL_DATE)
  df$monthnum = as.numeric(as.factor(months(date)))
  
  df$season = cut(df$monthnum,c(1,4,8,12))
  levels(df$season) = c("winter","spring","summer","fall")
  return(df)
}


add_binomial <- function(df,t)
{
  df$ISDELAYED = df$ARR_DELAY > t
  return(df)
}