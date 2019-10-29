# Incase we want to redo data extraction from main data set
# reads csv based on year input and outputs filtered csv
# can be modified for city and other filters
#
#
clean_data <- function(year) {
  infile = paste(toString(year), ".csv", sep="")
  data = read.csv(infile)
  
  data_boston = data[which(data$ORIGIN == "BOS" | data$DEST == "BOS"),]
  
  outfile = paste(toString(year), "_boston.csv", sep="")
  write.csv(data_boston, outfile)
}

for (year in seq(2009, 2018)) {
  print(paste("Cleaning year", year))
  clean_data(year)
}
