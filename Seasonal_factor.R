#will create a separate dataframe with seasons (spring,summer,fall,winter)
#julyaugseptember == summer
#septoctnov == fall
#janfebmarch == winter
#aprilmayjune == spring

plot(train_2018$FL_DATE,train_2018$DEP_DELAY)



seasonal_data <- function(df)
{
date <- as.Date(df$FL_DATE)
df$monthnum = as.numeric(as.factor(months(date)))

df$season = cut(df$monthnum,c(1,4,8,12))
levels(df$season) = c("winter","spring","summer","fall")
return(df)
}
# 
#                             
#   for (i in df$monthnum) {
#   date[i] <- as.Date(train_2018$FL_DATE)
#   months(date[1])
#   temp
#   if(df$monthnum[i] == 1|2|3|4 )
#     df$season[i] = 
# #   
#   
# }


# flight_Bos_2018$time_of_day="TBD"
# flight_Bos_2018$time_of_day=cut(flight_Bos_2018$CRS_DEP_TIME,c(0,600,1200,1800,2400))
# levels(flight_Bos_2018$time_of_day) = c("night","morning","afternoon","evening")