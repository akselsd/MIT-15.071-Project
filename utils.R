

add_timeofday_column <- function(df) {
  df$TIME_OF_DAY=cut(df$CRS_DEP_TIME, c(0,600,1200,1800,2400))
  levels(df$TIME_OF_DAY) = c("night","morning","afternoon","evening")
  return(df)
}

