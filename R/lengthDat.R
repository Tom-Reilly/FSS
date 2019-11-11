lengthDat <- function(cruiseCodeSeries, species, as.List = FALSE) {

  cat("Extracting length data")

  cat("\n----------------------------------------------------------------------------\n")

  if(missing(cruiseCodeSeries)) {stop("Please enter the cruise or cruises you wish to extract data from")}
  if(is.vector(cruiseCodeSeries) && length(cruiseCodeSeries) > 2) {stop("Please only supply two elements to the vector")}

  # Set switch based on whether a vector or dataframe has been supplied for cruiseCodeSeries
  if(is.vector(cruiseCodeSeries)) {codeSwitch <- 0}
  if(is.data.frame(cruiseCodeSeries)) {codeSwitch <- 1}

  # Set up the part of the SQL query that deals with the species portion of the query
  if(missing(species)) {speciesQuery <- "' GROUP BY fldCruiseName, fldCruiseStationNumber, fldGearCode, fldMainSpeciesCode, fldSex, fldLengthGroup, fldLengthGroupRaisingFactor"
  } else {
    speciesQuery <- paste("' AND fldMainSpeciesCode IN ('", paste(species, collapse = "','"), "') GROUP BY fldCruiseName, fldCruiseStationNumber, fldGearCode, fldMainSpeciesCode, fldSex, fldLengthGroup, fldLengthGroupRaisingFactor", sep = "")
  }

  # Set up the main part of the length frequency SQL query
  lfrqQuery <- paste("
  SELECT
  fldCruiseName AS CruiseName,
  fldCruiseStationNumber AS Haul,
	fldGearCode AS GearCode,
	fldMainSpeciesCode AS Species,
	fldSex AS Sex,
	fldLengthGroup AS Length,
  SUM(fldMeasuredNumberAtLength) AS MeasuredNumber,
  fldLengthGroupRaisingFactor AS RaisingFactor,
	SUM(fldCategoryRaisedNumberAtLength) AS RaisedNumber
	 FROM dbo.tblDataLengthSamples
	 WHERE fldCruiseName", sep="")

  # Set up gear SQL query
  gearQuery <- paste("SELECT fldGearCode AS GearCode, fldGearDescription AS GearName FROM dbo.tblReferenceMainGearCodes")

  ################################################################################################

  # Extract data on cruise selection if codeSwitch = 0 (only one cruise is supplied)
  if(codeSwitch == 0) {

    # Take the cruise code and cruise series from cruiseCodeSeries regardless of their order in the vector
    cruiseCode <- cruiseCodeSeries[grepl("[0-9]{4}", cruiseCodeSeries)]
    cruiseSeries <- cruiseCodeSeries[grepl("^[A-Z]{1,15}[0-9]{0,1}$", cruiseCodeSeries)]

    # Pick out table to reference
    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")
    cruiseSeries <- cruiseTable$fldSeriesName[grep(cruiseCode, cruiseTable$fldCruiseName)]

    # Set up the SQL query text
    lfrqQuery <- paste(lfrqQuery, "='", cruiseCode, speciesQuery, sep = "")
    lfrqQuery <- gsub('\n', '', lfrqQuery)
    lfrqData <- sqlQuery(channel, lfrqQuery)
    if(nrow(lfrqData) == 0) {stop(paste("The particular cruise/species combination contains no data: ", cruiseCode, sep = ""))}

    usedGearCodes <- unique(lfrqData$GearCode)
    for(g in 1:length(usedGearCodes)){
      gearCodes <- sqlQuery(channel, gearQuery)
    }
    rm(g)

    lfrqData <- merge(lfrqData, gearCodes, by.x = "GearCode", by.y = "GearCode")
    lfrqData <- lfrqData[,c("CruiseName","Haul","GearCode","GearName","Species","Length","MeasuredNumber","RaisingFactor","RaisedNumber")]
    lfrqData$RaisedNumber <- round(lfrqData$RaisedNumber)

    if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCode & cruiseTable$fldSeriesName == cruiseSeries] == "N") {
      message("Warning: The requested cruise has not been closed and therefore has received no quality checks")
    }

  }

  ################################################################################################

  # Extract data on cruise selection if codeSwitch = 1 (a dataframe of cruises is supplied)
  if(codeSwitch == 1) {

    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")

    notClosedCruises <- NULL

    lfrqDataList <- list()

    for(j in 1:nrow(cruiseCodeSeries)) {
      lfrqQueryTemp <- NULL
      lfrqDataTemp <- NULL
      lfrqQueryTemp <- paste(lfrqQuery, "='", cruiseCodeSeries$fldCruiseName[j], speciesQuery, sep = "")
      lfrqQueryTemp <- gsub('\n', '', lfrqQueryTemp)
      lfrqDataTemp <- sqlQuery(channel, lfrqQueryTemp)
      if(nrow(lfrqDataTemp) == 0) {message(paste("The particular cruise/species combination contains no data: ", cruiseCodeSeries$fldCruiseName[j], sep = ""))
                                   next
                                  }
      lfrqDataTemp$RaisedNumber <- round(as.numeric(lfrqDataTemp$RaisedNumber))
      usedGearCodes <- unique(lfrqDataTemp$GearCode)
      for(g in 1:length(usedGearCodes)){
        gearCodes <- sqlQuery(channel, gearQuery)
      }
      rm(g)
      lfrqDataList[[j]] <- merge(lfrqDataTemp, gearCodes, by.x = "GearCode", by.y = "GearCode")
      lfrqDataList[[j]] <- lfrqDataList[[j]][,c("CruiseName","Haul","GearCode","GearName","Species","Length","MeasuredNumber","RaisingFactor","RaisedNumber")]
      names(lfrqDataList)[[j]] <- paste(cruiseCodeSeries$fldCruiseName[j], cruiseCodeSeries$fldSeriesName[j], sep = " ")

      if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCodeSeries$fldCruiseName[j] & cruiseTable$fldSeriesName == cruiseCodeSeries$fldSeriesName[j]] == "N") {
        notClosedCruises <- c(notClosedCruises, cruiseCodeSeries$fldCruiseName[j])
      }
    }
    lfrqDataList <- lfrqDataList[!sapply(lfrqDataList, is.null)]

    notClosedAsString <- paste(notClosedCruises, collapse = " ")
    warningMessage <- paste("Warning: The following requested cruises have not been closed and therefore have received no quality checks:", notClosedAsString, sep = " ")

    if(! is.null(notClosedCruises)) {message(warningMessage)}
  }

  ################################################################################################

  if(as.List == FALSE & codeSwitch == 1) {
    lfrqDataTab <- lfrqDataList
    lastcol <- ncol(lfrqDataTab[[1]])
    for(j in 1:length(lfrqDataTab)) {
      lfrqDataTab[[j]][lastcol+1] <- rep(strsplit(names(lfrqDataTab), " ")[[j]][1], nrow(lfrqDataTab[[j]]))
      lfrqDataTab[[j]][lastcol+2] <- rep(strsplit(names(lfrqDataTab), " ")[[j]][2], nrow(lfrqDataTab[[j]]))
    }
    lfrqDataTab <- ldply(lfrqDataTab, data.frame, row.names = NULL)
    names(lfrqDataTab)[(lastcol+2):(lastcol+3)] <- c("CruiseName", "SeriesName")
    lfrqDataTab <- lfrqDataTab[,c(".id","CruiseName","SeriesName","Haul","GearCode","GearName","Species","Length","MeasuredNumber","RaisingFactor","RaisedNumber")]
    rm(j)
  }

  if(codeSwitch == 0) {return(lfrqData)}
  if(codeSwitch == 1 & as.List == TRUE) {return(lfrqDataList)}
  if(codeSwitch == 1 & as.List == FALSE) {return(lfrqDataTab)}

}
