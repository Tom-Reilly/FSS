cruiseSelector <- function(years, quarters, startMonth, series) {
  
  cat("Selecting cruises")
  
  cat("\n----------------------------------------------------------------------------\n")
  
  # Pick out table to reference
  cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")

  if(! missing(years)) {
    tempYears <- NULL
    
    # If any of the supplied years are in the format yyyy then change these to yy format
    for(i in 1:length(years)){
      if (nchar(years[i]) == 4){years[i] <- substr(years[i],3,4)}
      if (nchar(years[i]) == 2){tempYears[i] <- paste("20", years[i], sep = "")}   
    }
    # Remove counter
    rm(i)
  }
  
  if(missing(years)) {
    cat("Cruises not filtered by year")
  } else {
    cat(paste("Cruises selected from year(s): ", paste(tempYears[order(tempYears)], sep = "", collapse = ", "), sep = ""))
  }
  
  cat("\n----------------------------------------------------------------------------\n")
 
  tableYears <- str_extract(cruiseTable$fldCruiseName,"\\d+")
  
  # If the years argument is missing use all years in data table
  if(missing(years)) {
    years <- unique(substr(tableYears,3,4))
  }
  
  # Subset cruise table based on year selections
  cruiseTable <- cruiseTable[substr(tableYears,3,4) %in% years,]
  
  # Print text on screen to highlight which quarters have been selected from the cruise table
  if(missing(quarters)){
    cat("Cruises not filtered by quarter")
  } else {
    cat(paste("Cruises selected from quarter(s): ", paste(quarters[order(quarters)], sep = "", collapse = ", "), sep = ""))
  }
  
  cat("\n----------------------------------------------------------------------------\n")
  
  # If the quarters argument is missing use all quarters in data table
  if(missing(quarters)){quarters <- 1:4}
  
  # Subset cruise table based on quarter selections
  tempTab <- NULL
  
  if(1 %in% quarters){tempTab <- rbind(cruiseTable[format(cruiseTable$fldStartDate, "%m-%d") >= "01-01" & format(cruiseTable$fldStartDate, "%m-%d") <= "03-31",],tempTab)}
  if(2 %in% quarters){tempTab <- rbind(cruiseTable[format(cruiseTable$fldStartDate, "%m-%d") >= "04-01" & format(cruiseTable$fldStartDate, "%m-%d") <= "06-30",],tempTab)}
  if(3 %in% quarters){tempTab <- rbind(cruiseTable[format(cruiseTable$fldStartDate, "%m-%d") >= "07-01" & format(cruiseTable$fldStartDate, "%m-%d") <= "09-30",],tempTab)}
  if(4 %in% quarters){tempTab <- rbind(cruiseTable[format(cruiseTable$fldStartDate, "%m-%d") >= "10-01" & format(cruiseTable$fldStartDate, "%m-%d") <= "12-31",],tempTab)}
  
  cruiseTable <- tempTab
  
  # Print text on screen to highlight which start months have been selected from the cruise table
  if(missing(startMonth)){
    cat("Cruises not filtered by start month")
  } else {
    cat(paste("Cruises selected from start month(s): ", paste(startMonth[order(startMonth)], sep = "", collapse = ", "), sep = ""))
  }
  
  cat("\n----------------------------------------------------------------------------\n")
  
  # If the startMonth argument is missing use all months in data table
  if(missing(startMonth) && missing(quarters)){startMonth <- 1:12}
  
  if(missing(startMonth) && !(missing(quarters))){tempVec <- NULL}
  
  if(missing(startMonth) && 1 %in% quarters){tempVec <- c(1:3,tempVec)}
  if(missing(startMonth) && 2 %in% quarters){tempVec <- c(4:6,tempVec)}
  if(missing(startMonth) && 3 %in% quarters){tempVec <- c(7:9,tempVec)}
  if(missing(startMonth) && 4 %in% quarters){tempVec <- c(10:12,tempVec)}
  
  if(missing(startMonth) && !(missing(quarters))){startMonth <- tempVec}
  
  # Stop and print error message if the selected quarters and months are incompatible 
  if(any(1:3 %in% startMonth) && !(1 %in% quarters)){stop("The selected month(s) is/are incompatible with the selected quarter(s)")}
  if(any(4:6 %in% startMonth) && !(2 %in% quarters)){stop("The selected month(s) is/are incompatible with the selected quarter(s)")}
  if(any(7:9 %in% startMonth) && !(3 %in% quarters)){stop("The selected month(s) is/are incompatible with the selected quarter(s)")}
  if(any(10:12 %in% startMonth) && !(4 %in% quarters)){stop("The selected month(s) is/are incompatible with the selected quarter(s)")}
  
  # Subset cruise table based on start month selections
  tempTab <- NULL
  
  if(1 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "01",],tempTab)}
  if(2 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "02",],tempTab)}
  if(3 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "03",],tempTab)}
  if(4 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "04",],tempTab)}
  if(5 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "05",],tempTab)}
  if(6 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "06",],tempTab)}
  if(7 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "07",],tempTab)}
  if(8 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "08",],tempTab)}
  if(9 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "09",],tempTab)}
  if(10 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "10",],tempTab)}
  if(11 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "11",],tempTab)}
  if(12 %in% startMonth){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldStartDate,6,7) == "12",],tempTab)}
  
  cruiseTable <- tempTab
  
  # Print text on screen to highlight which series have been selected from the cruise table
  if(missing(series)){
    cat("Cruises not filtered by series")
  } else {                    
    cat(paste("Cruises selected from series('): ", paste(series[order(series)], sep = "", collapse = ", "), sep = ""))
  }
  
  cat("\n----------------------------------------------------------------------------\n")
  
  # If the series argument is missing use all series in data table
  if(missing(series)){series <- c("DEEPWATER", "GEARTRIALS", "HERAS", "MACAS", "MONKCHART", "MONKFISH", "NSIBTS", "ROCKALL", "SKATE", "WCIBTS","SIAMISS")}
  
  # Subset cruise table based on series selections
  tempTab <- NULL
  
  if("DEEPWATER" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "DEEPWATER",], tempTab)}
  if("GEARTRIALS" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "GEARTRIALS",], tempTab)}
  if("HERAS" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "HERAS",], tempTab)}
  if("MACAS" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "MACAS",], tempTab)}
  if("MONKCHART" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "MONKCHART",], tempTab)}
  if("MONKFISH" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "MONKFISH",], tempTab)}
  if("NSIBTS" %in% series){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldSeriesName,1,6) == "NSIBTS",], tempTab)}
  if("WCIBTS" %in% series){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldSeriesName,1,6) == "WCIBTS",], tempTab)}
  if("ROCKALL" %in% series){tempTab <- rbind(cruiseTable[substr(cruiseTable$fldSeriesName,1,7) == "ROCKALL",], tempTab)}
  if("SKATE" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "SKATE",], tempTab)}
  if("SIAMISS" %in% series){tempTab <- rbind(cruiseTable[cruiseTable$fldSeriesName == "SIAMISS",], tempTab)}
  
  cruiseTable <- tempTab
  
  cruiseTable[,1] <- as.character(cruiseTable[,1])
  cruiseTable[,7] <- as.character(cruiseTable[,7])
  
  return(cruiseTable[,c(1,7)])
  
}

