chronDat <- function(cruiseCodeSeries, as.List = FALSE) {
  
  cat("Extracting chron data")
  
  cat("\n----------------------------------------------------------------------------\n")
  
  if(missing(cruiseCodeSeries)) {stop("Please enter the cruise or cruises you wish to extract data from")}
  if(is.vector(cruiseCodeSeries) && length(cruiseCodeSeries) > 2) {stop("Please only supply two elements to the vector")}
  
  # Set switch based on whether a vector or dataframe has been supplied for cruiseCodeSeries
  if(is.vector(cruiseCodeSeries)) {codeSwitch <- 0}  
  if(is.data.frame(cruiseCodeSeries)) {codeSwitch <- 1}
  
  # Set up basis for main SQL query
  chronQuery <- paste("
                       SELECT 
                       dbo.tblDataStationLogs.fldCruiseName AS CruiseName, 
                       dbo.tblDataStationLogs.fldICESRectangle AS StatSquare,
                       dbo.tblDataStationLogs.fldCruiseStationNumber As Haul,
                       dbo.tblDataStationLogs.fldDateTimeShot AS TimeShot,
                       dbo.tblDataStationLogs.fldDateTimeHaul AS TimeHaul,
                       dbo.tblDataStationLogs.fldShotLatDecimalDegrees AS ShotLat,
                       dbo.tblDataStationLogs.fldShotLonDecimalDegrees AS ShotLon,
                       dbo.tblDataStationLogs.fldShotDepth AS ShotDepth,
                       dbo.tblDataStationLogs.fldHaulLatDecimalDegrees AS HaulLat,
                       dbo.tblDataStationLogs.fldHaulLonDecimalDegrees AS HaulLon,
                       dbo.tblDataStationLogs.fldHaulDepth AS HaulDepth,
                       dbo.tblDataStationLogs.fldStratum AS Stratum,
                       dbo.tblDataGearDeployments.fldGearCode AS GearCode,
                       dbo.tblDataGearDeployments.fldValidityCode AS Valid
                       FROM dbo.tblDataStationLogs INNER JOIN dbo.tblDataGearDeployments 
                       ON(dbo.tblDataStationLogs.fldCruiseName=dbo.tblDataGearDeployments.fldCruisename AND dbo.tblDataStationLogs.fldCruiseStationNumber=dbo.tblDataGearDeployments.fldCruiseStationNumber)
                       WHERE dbo.tblDataStationLogs.fldCruiseName", sep = "")
  
  # Set up basis for stratum SQL query
  stratumQuery <- paste("SELECT fldStratumID AS StratumID, fldStratumDescription AS StratumName FROM dbo.tblSeriesStratum WHERE fldSeriesName", sep = "")
  
  # Set up gear SQL query
  gearQuery <- paste("SELECT fldGearCode AS GearCode, fldGearDescription AS GearName FROM dbo.tblReferenceMainGearCodes")
 
  ################################################################################################
  
  # Extract data on cruise selection if codeSwitch = 0 (only one cruise is supplied)
  if(codeSwitch == 0){
    
    # Take the cruise code and cruise series from cruiseCodeSeries regardless of their order in the vector
    cruiseCode <- cruiseCodeSeries[grepl("[0-9]{4}", cruiseCodeSeries)]
    cruiseSeries <- cruiseCodeSeries[grepl("^[A-Z]{1,15}[0-9]{0,1}$", cruiseCodeSeries)]

    # Pick out table to reference
    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")
    cruiseSeries <- cruiseTable$fldSeriesName[grep(cruiseCode, cruiseTable$fldCruiseName)]

    # Temporarily extract the table that houses the series that have associated strata
    temp <- sqlFetch(channel, "dbo.tblSeriesStratum")
    
    # Set the stratumSwitch to be on (1) if the cruise has associated strata
    if(any(grepl(cruiseSeries, temp[["fldSeriesName"]]))){
      stratumSwitch <- 1
    } else {
      stratumSwitch <- 0}
    
    temp <- NULL
    
    # Set up the SQL query text
    chronQuery <- paste(chronQuery, "='", cruiseCode, "'", sep = "")
    chronQuery <- gsub('\n','',chronQuery)
    
    # If the cruise has no strata then this runs to get the cruise chron data
    if(stratumSwitch == 0) {
      chronData <- sqlQuery(channel, chronQuery, as.is = TRUE)
      usedGearCodes <- unique(chronData$GearCode)
      for(g in 1:length(usedGearCodes)){
        gearCodes <- sqlQuery(channel, gearQuery)
      }
      rm(g)
      chronData <- merge(chronData, gearCodes, by.x = "GearCode", by.y = "GearCode")
      chronData <- chronData[,c("CruiseName","Haul","StatSquare","GearCode","GearName","TimeShot",
                                "TimeHaul","ShotLat","ShotLon","HaulLat","HaulLon","HaulDepth","Valid")]
    }
    
    # If the cruise has strata then this runs to get the cruise chron data and attaches stratum data
    if(stratumSwitch == 1) {
      chronData <- sqlQuery(channel, chronQuery, as.is = TRUE)
      stratumQuery <- paste(stratumQuery, "='", cruiseSeries, "'", sep = "")
      stratumNames <- sqlQuery(channel, stratumQuery)
      chronData <- merge(chronData, stratumNames, by.x = "Stratum", by.y = "StratumID")
      usedGearCodes <- unique(chronData$GearCode)
      for(g in 1:length(usedGearCodes)){
        gearCodes <- sqlQuery(channel, gearQuery)
      }
      rm(g)
      chronData <- merge(chronData, gearCodes, by.x = "GearCode", by.y = "GearCode")
      chronData <- chronData[,c("CruiseName","Haul","StatSquare","GearCode","GearName","Stratum","StratumName","TimeShot",
                                "TimeHaul","ShotLat","ShotLon","HaulLat","HaulLon","HaulDepth","Valid")]
    }
    
    if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCode & cruiseTable$fldSeriesName == cruiseSeries] == "N") {
      message("Warning: The requested cruise has not been closed and therefore has received no quality checks")
    }
    
  }
  
  ################################################################################################
  
  # Extract data on cruise selection if codeSwitch = 1 (a dataframe of cruises is supplied)
  if(codeSwitch == 1) {
    
    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")
    
    notClosedCruises <- NULL
    
    # Temporarily extract the table that houses the series that have associated strata
    temp <- sqlFetch(channel, "dbo.tblSeriesStratum")
    
    for(i in 1:nrow(cruiseCodeSeries)){
      if(any(grepl(cruiseCodeSeries$fldSeriesName[i], temp[["fldSeriesName"]]))){
        cruiseCodeSeries$stratumSwitch[i] <- 1
      } else {
        cruiseCodeSeries$stratumSwitch[i] <- 0} 
    }
    rm(i)
    
    temp <- NULL
    
    chronDataList <- list()
    
    for(j in 1:nrow(cruiseCodeSeries)) {
      chronQueryTemp <- NULL
      chronDataTemp <- NULL
      if(cruiseCodeSeries$stratumSwitch[j] == 0) {
        chronQueryTemp <- paste(chronQuery, "='", cruiseCodeSeries$fldCruiseName[j], "'", sep = "")
        chronQueryTemp <- gsub('\n','',chronQueryTemp)
        chronDataTemp <- sqlQuery(channel, chronQueryTemp, as.is = TRUE)
        chronDataTemp$StratumName <- rep(NA, nrow(chronDataTemp))
        usedGearCodes <- unique(chronDataTemp$GearCode)
        for(g in 1:length(usedGearCodes)){
          gearCodes <- sqlQuery(channel, gearQuery)
        }
        rm(g)
        chronDataList[[j]] <- merge(chronDataTemp, gearCodes, by.x = "GearCode", by.y = "GearCode")
        chronDataList[[j]] <- chronDataList[[j]][,c("CruiseName","Haul","StatSquare","GearCode","GearName","Stratum","StratumName","TimeShot",
                                                    "TimeHaul","ShotLat","ShotLon","HaulLat","HaulLon","HaulDepth","Valid")]
        names(chronDataList)[[j]] <- paste(cruiseCodeSeries$fldCruiseName[j], cruiseCodeSeries$fldSeriesName[j], sep = " ")
        
        if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCodeSeries$fldCruiseName[j] & cruiseTable$fldSeriesName == cruiseCodeSeries$fldSeriesName[j]] == "N") {
          notClosedCruises <- c(notClosedCruises, cruiseCodeSeries$fldCruiseName[j])
        }
          
      }
      if(cruiseCodeSeries$stratumSwitch[j] == 1){
        chronQueryTemp <- paste(chronQuery, "='", cruiseCodeSeries$fldCruiseName[j], "'", sep = "")
        chronQueryTemp <- gsub('\n', '', chronQueryTemp)
        chronDataTemp <- sqlQuery(channel, chronQueryTemp, as.is = TRUE)
        stratumQueryTemp <- paste(stratumQuery, "='", cruiseCodeSeries$fldSeriesName[j], "'", sep = "")
        stratumNames <- sqlQuery(channel, stratumQueryTemp)
        chronDataTemp <- merge(chronDataTemp, stratumNames, by.x = "Stratum", by.y = "StratumID")
        usedGearCodes <- unique(chronDataTemp$GearCode)
        for(g in 1:length(usedGearCodes)){
          gearCodes <- sqlQuery(channel, gearQuery)
        }
        rm(g)
        chronDataList[[j]] <- merge(chronDataTemp, gearCodes, by.x = "GearCode", by.y = "GearCode")
        chronDataList[[j]] <- chronDataList[[j]][,c("CruiseName","Haul","StatSquare","GearCode","GearName","Stratum","StratumName","TimeShot",
                                                    "TimeHaul","ShotLat","ShotLon","HaulLat","HaulLon","HaulDepth","Valid")]
        names(chronDataList)[[j]] <- paste(cruiseCodeSeries$fldCruiseName[j], cruiseCodeSeries$fldSeriesName[j], sep = " ")
        
        if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCodeSeries$fldCruiseName[j] & cruiseTable$fldSeriesName == cruiseCodeSeries$fldSeriesName[j]] == "N") {
          notClosedCruises <- c(notClosedCruises, cruiseCodeSeries$fldCruiseName[j])
        }
          
      }
      
    }
    rm(j)
    
    notClosedAsString <- paste(notClosedCruises, collapse = " ")
    warningMessage <- paste("Warning: The following requested cruises have not been closed and therefore have received no quality checks:", notClosedAsString, sep = " ")
    
    if(! is.null(notClosedCruises)) {message(warningMessage)}
    
  }
  
  ################################################################################################
  
  if(codeSwitch == 1 & as.List == FALSE) {
    chronDataTab <- chronDataList
    lastcol <- ncol(chronDataTab[[1]])
    for(j in 1:nrow(cruiseCodeSeries)) {
      chronDataTab[[j]][lastcol+1] <- rep(cruiseCodeSeries$fldCruiseName[j], nrow(chronDataTab[[j]]))
      chronDataTab[[j]][lastcol+2] <- rep(cruiseCodeSeries$fldSeriesName[j], nrow(chronDataTab[[j]]))
    }
    chronDataTab <- dplyr::bind_rows(chronDataTab)
    names(chronDataTab)[(lastcol+1):(lastcol+2)] <- c("CruiseName","SeriesName")
    chronDataTab <- chronDataTab[,c("CruiseName","SeriesName","Haul","StatSquare","GearCode","GearName","Stratum","StratumName","TimeShot",
                                    "TimeHaul","ShotLat","ShotLon","HaulLat","HaulLon","HaulDepth","Valid")]
    rm(j)
  }
  
  if(codeSwitch == 0) {return(chronData)}
  if(codeSwitch == 1 & as.List == TRUE) {return(chronDataList)}
  if(codeSwitch == 1 & as.List == FALSE) {return(chronDataTab)}
  

}

