bioDat <- function(cruiseCodeSeries, species, as.List = FALSE) {
  
  cat("Extracting biological data")
  
  cat("\n----------------------------------------------------------------------------\n")
  
  if(missing(cruiseCodeSeries)) {stop("Please enter the cruise or cruises you wish to extract data from")}
  if(is.vector(cruiseCodeSeries) && length(cruiseCodeSeries) > 2) {stop("Please only supply two elements to the vector")}
  
  # Set switch based on whether a vector or dataframe has been supplied for cruiseCodeSeries
  if(is.vector(cruiseCodeSeries)) {codeSwitch <- 0}  
  if(is.data.frame(cruiseCodeSeries)) {codeSwitch <- 1}
  
  # Set up the part of the SQL query that deals with the species portion of the query
  if(missing(species)) {speciesQuery <- "'"
  } else {                     
    speciesQuery <- paste("' AND fldMainSpeciesCode IN ('", paste(species, collapse = "','"), "')", sep = "")
  }
  
  # Set up the main part of the biological SQL query
  bioQuery <- paste("
  SELECT 
  fldCruiseName AS CruiseName,
  fldCruiseStationNumber AS Haul,
  fldGearCode AS GearCode,
	fldMainSpeciesCode AS Species, 
	dbo.tblReferenceMainSpecies.fldScientificName AS LatinName,
  fldInternalBiologicalSampleID AS SampleID,
	fldFishSex AS Sex,
	fldFishLength/10 AS Length,
  fldFishWholeWeight AS WholeWeight,
  fldLinkSampleID AS GuttedWeight,
  fldFishMaturity AS Maturity,
  fldResult1 AS Age
	 FROM dbo.tblDataBiologicalSamples
	 JOIN dbo.tblReferenceMainSpecies on dbo.tblDataBiologicalSamples.fldMainSpeciesCode = dbo.tblReferenceMainSpecies.fldMainSpeciesCode
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
    bioQuery <- paste(bioQuery, "='", cruiseCode, speciesQuery, sep = "")
    bioQuery <- gsub('\n', '', bioQuery)  
    bioData <- sqlQuery(channel, bioQuery)
    if(nrow(bioData) == 0) {stop(paste("The particular cruise/species combination contains no data: ", cruiseCode, sep = ""))} 
    
    usedGearCodes <- unique(bioData$GearCode)
    for(g in 1:length(usedGearCodes)){
      gearCodes <- sqlQuery(channel, gearQuery)
    }
    rm(g)
    
    bioData <- merge(bioData, gearCodes, by.x = "GearCode", by.y = "GearCode")
    bioData <- bioData[,c("CruiseName","Haul","GearCode","GearName","SampleID","Species","LatinName","Length","WholeWeight",
                          "GuttedWeight","Sex","Maturity","Age")]
    
    if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCode & cruiseTable$fldSeriesName == cruiseSeries] == "N") {
      message("Warning: The requested cruise has not been closed and therefore has received no quality checks")
      }
    
  }
  
  ################################################################################################
  
  bioDataList <- list()
  
  # Extract data on cruise selection if codeSwitch = 1 (a dataframe of cruises is supplied)
  if(codeSwitch == 1) {
    
    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")
    
    notClosedCruises <- NULL
    
    for(j in 1:nrow(cruiseCodeSeries)) {
      bioQueryTemp <- NULL
      bioDataTemp <- NULL
      bioQueryTemp <- paste(bioQuery, "='", cruiseCodeSeries$fldCruiseName[j], speciesQuery, sep = "")
      bioQueryTemp <- gsub('\n', '', bioQueryTemp)
      bioDataTemp <- sqlQuery(channel, bioQueryTemp)
      if(nrow(bioDataTemp) == 0) {message(paste("The particular cruise/species combination contains no data: ", cruiseCodeSeries$fldCruiseName[j], sep = "")) 
                                   next
      }
      usedGearCodes <- unique(bioDataTemp$GearCode)
      for(g in 1:length(usedGearCodes)){
        gearCodes <- sqlQuery(channel, gearQuery)
      }
      rm(g)
      bioDataList[[j]] <- merge(bioDataTemp, gearCodes, by.x = "GearCode", by.y = "GearCode")
      bioDataList[[j]] <- bioDataList[[j]][c("CruiseName","Haul","GearCode","GearName","SampleID","Species","LatinName","Length","WholeWeight",
                                             "GuttedWeight","Sex","Maturity","Age")]
      names(bioDataList)[[j]] <- paste(cruiseCodeSeries$fldCruiseName[j], cruiseCodeSeries$fldSeriesName[j], sep = " ")
      
      if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCodeSeries$fldCruiseName[j] & cruiseTable$fldSeriesName == cruiseCodeSeries$fldSeriesName[j]] == "N") {
        notClosedCruises <- c(notClosedCruises, cruiseCodeSeries$fldCruiseName[j])
      }
      
    }
    
    bioDataList <- bioDataList[!sapply(bioDataList, is.null)]

    notClosedAsString <- paste(notClosedCruises, collapse = " ")
    warningMessage <- paste("Warning: The following requested cruises have not been closed and therefore have received no quality checks:", notClosedAsString, sep = " ")

    if(! is.null(notClosedCruises)) {message(warningMessage)}
  }
  
  ################################################################################################
  
  if(codeSwitch == 1 & as.List == FALSE & length(bioDataList) == 0) {return(NULL)}
  
  if(as.List == FALSE & codeSwitch == 1) {
    bioDataTab <- bioDataList
    lastcol <- ncol(bioDataTab[[1]])
    for(j in 1:length(bioDataTab)) {
      bioDataTab[[j]][lastcol+1] <- rep(strsplit(names(bioDataTab), " ")[[j]][1], nrow(bioDataTab[[j]]))
      bioDataTab[[j]][lastcol+2] <- rep(strsplit(names(bioDataTab), " ")[[j]][2], nrow(bioDataTab[[j]]))
    }
    bioDataTab <- ldply(bioDataTab, data.frame, row.names = NULL)
    names(bioDataTab)[(lastcol+2):(lastcol+3)] <- c("CruiseName", "SeriesName")
    bioDataTab <- bioDataTab[,c(".id","CruiseName","SeriesName","Haul","GearCode","GearName","SampleID","Species","LatinName","Length","WholeWeight",
                                "GuttedWeight","Sex","Maturity","Age")]
    rm(j)
  }
  
  if(codeSwitch == 0) {return(bioData)}
  if(codeSwitch == 1 & as.List == TRUE) {return(bioDataList)}
  if(codeSwitch == 1 & as.List == FALSE) {return(bioDataTab)}
  
}
