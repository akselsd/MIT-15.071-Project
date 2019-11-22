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


#the function below includes cancelled flights in the data set and zeros them instead of NA
#and is used in the main function load_data

load_data <- function(startyear, endyear) {
  
  assert("Endyear is greater than or equal to startyear", endyear >= startyear)
  infile = paste("Data/", toString(startyear), "_boston.csv", sep="")
  df = read.csv(infile)
  
  for (year in seq(startyear + 1, endyear)) {
    infile = paste("Data/", toString(year), "_boston.csv", sep="")
    data = read.csv(infile)
    df = rbind(df, data)
  }
  
  
  
  df = df[df$DIVERTED == 0,]
  df = df[df$CANCELLED == 0,]
  df$ARR_DELAY[df$ARR_TIME == df$CRS_ARR_TIME] <- 0
  df$DEP_DELAY[df$DEP_TIME == df$CRS_DEP_TIME] <- 0
  
  drops = c("DIVERTED", "Unnamed..27", "X")
  df = df[, !(names(df) %in% drops)]
  
  df <- subset(df, select = 
                 c(FL_DATE, OP_CARRIER, OP_CARRIER_FL_NUM, 
                   CRS_DEP_TIME, CRS_ARR_TIME, CRS_ELAPSED_TIME,
                   DISTANCE, ORIGIN, DEST, ARR_DELAY))
  
  
  carriers = c("AA", "AS", "B6", "DL", "EV", "UA", "WN")
  df = df[(df$OP_CARRIER %in% carriers),]
  df$OP_CARRIER = droplevels(df$OP_CARRIER)

  
  origins = "ACK|ATL|AUS|BNA|BOS|BUF|BWI|CHS|CLE|CLT|CMH|CVG|DAL|DCA|DEN|DFW|DTW|EWR|FLL|HOU|IAD|IAH|IND|JAX|JFK|LAS|LAX|LGA|LGB|MCI|MCO|MDW|MIA|MKE|MSP|MSY|MVY|OAK|ORD|PBI|PDX|PHL|PHX|PIT|RDU|RIC|RSW|SAN|SAV|SEA|SFO|SJC|SJU|SLC|SMF|SRQ|STL|STT|TPA|SYR"
  destinations = "ACK|ATL|AUS|BNA|BOS|BUF|BWI|CHS|CLE|CLT|CMH|CVG|DAL|DCA|DEN|DFW|DTW|EWR|FLL|HOU|IAD|IAH|IND|JAX|JFK|LAS|LAX|LGA|LGB|MCI|MCO|MDW|MIA|MKE|MSN|MSP|MSY|MVY|OAK|ORD|ORF|PBI|PDX|PHL|PHX|PIT|RDU|RIC|RSW|SAN|SAV|SEA|SFO|SJC|SJU|SLC|SMF|SRQ|STL|STT|TPA"
  
  originvec = unlist(strsplit(origins, "|", fixed=TRUE))
  destvec = unlist(strsplit(destinations, "|", fixed=TRUE))
  
  df = df[(df$DEST %in% destvec),]
  df$DEST = droplevels(df$DEST)
  
  df = df[(df$ORIGIN %in% originvec),]
  df$ORIGIN = droplevels(df$ORIGIN)
  
  
  
  
  df = add_timeofday_column(df)
  df = add_seasonal_data(df)
  df$WEEKDAY = factor(weekdays(as.Date(df$FL_DATE,'%Y-%m-%d')))

  return(df)
}