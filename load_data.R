#
# Example usage: 
#
# source("load_data.R")           # Import this function
# train = load_data(2013, 2017)   # Training data from 2013 to 2017
# test = load_data(2018, 2018)    # Testing data, 2018 only
#
load_data <- function(startyear, endyear) {
  df = data.frame()
  
  for (year in seq(startyear, endyear)) {
    infile = paste("Data/", toString(startyear), "_boston.csv", sep="")
    data = read.csv(infile)
    df = rbind(df, data)
  }
  
  return(df)
}