#Script Info####
# Date: 22 Oct 2015
# By: Laura Stupin
# Purpose: Compile statistics for batches
# Reading new Master Spreadsheet - ms80_trike_lab_all.csv and water.csv

# History: Descended from BatchTrack5.R

# Clear workspace
rm(list = ls() ) 

#Functions and Libraries####


# Load functions ReadNewExcel, ReadRefData, AddToRawTable from another R script
source("../read_in_excel.R")

library("dplyr")

#Cost of Water####
  # Load cost and assumptions csv
  assmpt <- ReadRefData("costs and assumptions")
  
  # Create a vector of parameters, each number is named.
  params <- assmpt$value
  names(params) <- assmpt$name
  
  # Load water data csv, rename columns
  water <- ReadNewCSV("water")
  names(water) <- c("date.time", "vol", "floc.added", "notes")
  
  # Subtract 1.5 m3 from each volume - that is wasted as stuff on the bottom
  water$usable.vol <- water$vol - 1.5
  
  kg.floc <- sum(water$floc.added)
  water.cost <- params["floc.rwf"]*kg.floc/
               sum(water$usable.vol)
  water.cost <- round(water.cost, 0)
  
  #Update the average cost of water
  assmpt$value[assmpt$name == "water.rwf"] <- water.cost
  assmpt$last.update[assmpt$name =="water.rwf"] <- as.character(as.Date(max(water$date.time)))
  write.csv(assmpt, 
            file="../../../Kigali Raw Data/Data Reference/costs and assumptions.csv", 
            row.names=FALSE)


#Master Spreadsheet####
  
  # Read in csv... m is short for "master"
  m <- read.csv("../../../Kigali Raw Data/Data New/csv/ms80_trike_lab_all.csv", as.is = T)  # Read in the csv to a data frame
    
  # Change the column names
  names(m) <-c("batch", "dc.notes", "origin", "ms80.date", "meter.start", 
               "tank.start", "water.start", "start.time", "add.wash.hi", 
               "add.wash.low", "tank.end", "water.end", "meter.end", 
               "stop.time", "fs.added", "cake.kg", "trike.time", "why.stop", "ms80.notes",
               "ms80.sludge.processed", "ms80.kWh", "ms80.water", "ms80.runtime",
               "sludge.m3.hr", "DS.kg", "DS.kg.hr", "zilch", "check", "cake.ts",
               "influ.ts", "efflu.ts", "pump.ts", "scraper.ts", "cavity.ts",
               "auger.ts", "cake.cv", "cake.ash", "beep", "boop", "bop")
  

  # Change all NA's to zeros... otherwise adding/subtracting NA's = NA
  m[is.na(m)] <- 0
  
  # Make sure there's no mistyped ms80s or inlets
  check <- (m$origin == "ms80") | (m$origin == "inlet") | (m$origin == "")
  trouble.makers <- m$batch[!check]
  
  if(sum(check) != nrow(m)){
      message(paste0("WARNING: There's an ms80/inlet typo in the Origin column of ms80_trike_lab_all.xlsx"))
      writeLines("Please check these following batches and look for a typo or an extra space: ")
      print(trouble.makers) 
  }
  
  # Build a new dataframe with the info we want
  # s stands for selected and shaped...
  # This next code section uses "pipes" the dataframe m is passed through a series of
  # commands without needing to relist/reassign m every time.
  
  shaped <- as.data.frame(m %>%
                    filter(origin == "ms80") %>%          #Look only at data where ms80 is listed in the origin
                    select(batch, ms80.date, start.time, ms80.sludge.processed, 
                           sludge.m3.hr, ms80.kWh, ms80.runtime, ms80.water, DS.kg,
                           DS.kg.hr) %>%                                          # Choose the columns we want to work with
                    mutate(kWh.m3.FS = ms80.kWh/ms80.sludge.processed) %>%        # Create a column with a new calculation
                    mutate(water.m3.FS = ms80.water/ms80.sludge.processed) %>%
                    mutate(kWh.RWF.m3.FS = params["electricity"]*kWh.m3.FS) %>%
                    mutate(water.RWF.m3.FS = params["water.rwf"]*water.m3.FS) %>%
                    mutate(kWh.kg.DS = ms80.kWh/DS.kg) %>%
                    mutate(water.kg.DS = ms80.water/DS.kg) %>%
                    mutate(kWh.RWF.kg.DS = params["electricity"]*kWh.kg.DS) %>%
                    mutate(water.RWF.kg.DS = params["water.rwf"]*water.kg.DS) %>%
                    select(batch, ms80.date, start.time, ms80.sludge.processed, sludge.m3.hr,
                           kWh.m3.FS, water.m3.FS, kWh.RWF.m3.FS, water.RWF.m3.FS,
                           kWh.kg.DS, water.kg.DS, kWh.RWF.kg.DS, water.RWF.kg.DS, 
                           ms80.kWh, ms80.water)                                  # Select and reorder columns
                    )


  # Round most columns to 2 decimal places
  shaped[6:ncol(shaped)] <- round(shaped[6:ncol(shaped)], 2)
  
  # Write out a csv with the most common calculations
  write.csv(shaped, file="../../../Kigali Data Reports/R outputs/batch_story.csv", row.names=FALSE)  
  

