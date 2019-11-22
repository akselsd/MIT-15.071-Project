par("mar")
par(mar=c(1,1,1,1))

plot(flight_Bos_2017$ARR_DELAY,flight_Bos_2017$CRS_ELAPSED_TIME)  
plot(flight_Bos_2017$FL_DATE, flight_Bos_2017$DEP_DELAY, xlab = "Date", ylab = "Delay Time")
plot(flight_Bos_2017$FL_DATE, flight_Bos_2017$WEATHER_DELAY, xlab = "Date", ylab = "WEATHER DELAY")
plot(flight_Bos_2017$FL_DATE, flight_Bos_2017$NAS_DELAY, xlab = "Date", ylab = "WEATHER DELAY")
plot(flight_Bos_2017$FL_DATE, flight_Bos_2017$SECURITY_DELAY, xlab = "Date", ylab = "WEATHER DELAY")
plot(flight_Bos_2017$FL_DATE, flight_Bos_2017$LATE_AIRCRAFT_DELAY, xlab = "Date", ylab = "WEATHER DELAY")
delayed_flights_BOS_2017<-subset(flight_Bos_2017, DEP_DELAY>14)
no_delay<-subset(flight_Bos_2017, DEP_DELAY<15)
delay_count<-table(delayed_flights_BOS_2017$OP_CARRIER)
no_delay_count<-table(no_delay$OP_CARRIER)
barplot(no_delay_count, main="on time vs carrier",
        xlab="carrier")
barplot(delay_count, main="delay vs carrier",
        xlab="carrier")


