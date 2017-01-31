#Script Info####
# Date: 15 Oct 2015
# By: Laura Stupin
# Purpose: Functions for data reading and shaping tasks

# History: Descended from read_new_ref_add_to_table3.R

library(XLConnect)

# file.name <- "ms80_machine"
# drops=NULL
# keeps= NULL

ReadNewExcel <- function(file.name, drops=NULL, keeps=NULL) {
  
  # File location
  full.address <- paste0("../../../Kigali Raw Data/Data New/excel/",  
                         file.name, ".xlsx")
  
  # print(drops)
  
  # Read first page into dataframe
  df <- readWorksheet(loadWorkbook(full.address), sheet=1, drop=drops, keep=keeps)
  names(df)[1:2] <- c("Date", "Time")      # Change the names of the first two columns

  # Unfortunately R reads in the time as a date/time at 1901-01-01
  # This code chunk is just to strip out the time, add it to the real date
  x <- as.character(df$Time)
  df$Time <- substr(x, nchar(x)-8+1, nchar(x)-3)
    date.time <- as.POSIXct(paste(as.character(df$Date), df$Time),   # Combine date and time columns.
                          format= "%Y-%m-%d %H:%M")
  df <- cbind(date.time, df)                         # Add new date and time column to beginning of data frame
  
  # Check for missing date or time data
  # Find rows where date.time is NA, and there is either no date value or time value
  date.or.time.missing <- is.na(df$date.time) & ( is.na(df[2]) | is.na(df[3]) )
  
  if(sum(date.or.time.missing) > 0){
    message(paste0("WARNING: ", file.name, ".xlsx is missing date or time info."))
    writeLines("Please check these rows in the file, they are not included in 
               analysis because they are missing a date or a time: ")
    print(which(date.or.time.missing)) 
  
  }
  
  
  df[3] <- NULL                                    # Drop the time column
  df[2] <- NULL                                    # Drop the date column
  df <- df[!(is.na(df$date.time)), ]               # Drop any rows that have NA in date.time column
  
  return(df)
}

ReadNewCSV <- function(file.name) {
  
  
  # Default address for new data
  full.address <- paste0("../../../Kigali Raw Data/Data New/csv/",  
                         file.name, ".csv")
  

  df <- read.csv(full.address, as.is = T)  # Read in the csv to a data frame
  names(df)[1:2] <- c("Date", "Time")      # Change the names of the first two columns
  
  
  # Check date format (it changes depending on who saves the .csv file)
  boo <- df[1,1]
  
  # Select the 3rd character from the end of the string
  slash <- substr(boo, nchar(boo)-2, nchar(boo)-2)
  
  # Check if the third character is a slash
  if (slash == "/") {
    date.format <- "%m/%d/%y %H:%M"   #%y is 2 digit year
  } else {
    date.format <- "%m/%d/%Y %H:%M"   #%Y is 4 digit year
  }
  
  
  date.time <- as.POSIXct(paste(df$Date, df$Time),   # Combine date and time columns.
                          format= date.format)
  df <- cbind(date.time, df)                         # Add new date and time column to beginning of data frame
  
  # Check for missing date or time data
  # Find rows where date.time is NA, and there is either no date value or time value
  date.or.time.missing <- is.na(df$date.time) & (df[2] == "" | df[3] == "")
  
  if(sum(date.or.time.missing) > 0){
    message(paste0("WARNING: ", file.name, ".csv is missing date or time info."))
    writeLines("Please check these rows in the file, they are not included in 
               analysis because they are missing a date or a time: ")
    print(which(date.or.time.missing)) 
    
  }
  
  
  df[3] <- NULL                                    # Drop the time column
  df[2] <- NULL                                    # Drop the date column
  df <- df[!(is.na(df$date.time)), ]               # Drop any rows that have NA in date.time column
  
  return(df)
}



ReadRefData <- function(file.name) {
  full.address <- paste0("../../../Kigali Raw Data/Data Reference/",  
                         file.name, ".csv")
  df <- read.csv(full.address, as.is = T)  # Read in the csv to a data frame
  
  return(df)
}

# This function appends new rows of data to a raw data table. 
# For example, 
# 
# date type amount
# 2015-07-04 inlet sludge 10.0
# 2015-07-13 inlet sludge 10.0
# 2015-07-14 inlet sludge 20.0
# 
# becomes
# 
# date type amount
# 2015-07-04 inlet sludge 10.0
# 2015-07-13 inlet sludge 10.0
# 2015-07-14 inlet sludge 20.0
# 2015-07-04 ms80 sludge 10.0
# 2015-07-14 ms80 sludge 20.0
# 2015-07-14 ms80 sludge 20.0
# 
# This is an important step for pulling data from different sources and creating a summarized 
# pivot table.

# Notes about inputs:
# main.df - the large table that is being expanded
# main.df must be initialized before this function is called with the correct column names
# new.df - the dataframe with info being extracted to add onto main.df
# label - a character string that describes the data such as "inlet sludge" or "ms80 water"
# data.name - name of the data being included, i.e. inlet$sludge.received

# main.df = consumables
# new.df = trucks
# label = "inlet"
# data.name = trucks$measured.vol

AddToRawTable <- function(main.df, new.df, label, data.name) {
  
  # Create a vector with just the label repeated i.e. "inlet", "inlet", "inlet", etc.
  label.vec <- rep(label, nrow(new.df))
  
  # Create the dataframe that will be appended
  temp <- data.frame(new.df$date, label.vec, data.name)
  
  # Change the names to match main.df
  names(temp) <- names(main.df)
  
  # Add inlet label vector to new data frame
  main.df <- rbind(main.df, temp)
  
  return(main.df)
}

# file.name <- "trucks"


