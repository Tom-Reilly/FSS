writeDATRAS <- function(cruiseCodeSeries, path) {

cat("Extracting DATRAS data")

cat("\n----------------------------------------------------------------------------\n")

  if(missing(cruiseCodeSeries)) {stop("Please enter the cruise or cruises you wish to extract data from")}
  if(is.vector(cruiseCodeSeries) && length(cruiseCodeSeries) > 2) {stop("Please only supply two elements to the vector")}
  if(missing(path)) {stop("Please provide a file path to write to")}

  # Set switch based on whether a vector or dataframe has been supplied for cruiseCodeSeries
  if(is.vector(cruiseCodeSeries)) {codeSwitch <- 0}
  if(is.data.frame(cruiseCodeSeries)) {codeSwitch <- 1}

  # if vector of single cruise
  if(codeSwitch == 0) {

    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")

    # Take the cruise code and cruise series from cruiseCodeSeries regardless of their order in the vector
    cruiseCode <- cruiseCodeSeries[grepl("[0-9]{4}", cruiseCodeSeries)]
    cruiseSeries <- cruiseCodeSeries[grepl("^[A-Z]{1,15}[0-9]{0,1}$", cruiseCodeSeries)]

    cruiseInfo <- getCruiseInfo(cruiseSeries)

    vesselName <- sqlQuery(channel, vessel_qry(cruiseCode))
    myVessel <- gsub("\\s", "", vesselName[,1])

    chronData <- sqlQuery(channel, chron_qry(cruiseCode))

    op <- file(paste(path, cruiseCode, ".txt", sep = ""), "w")

    hhheadline <- paste("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "Month",
                  "Day", "TimeShot", "DepthStratum", "HaulDur", "DayNight", "ShootLat", "ShootLong", "HaulLat", "HaulLong", "StatRec", "Depth",
                  "HaulVal", "HydroStNo", "StdSpecRecCode", "BySpecRecCode", "DataType", "Netopening", "Rigging", "Tickler", "Distance", 
                  "Warplngt", "Warpdia", "WarpDen", "DoorSurface", "DoorWgt", "DoorSpread", "WingSpread", "Buoyancy", "KiteDim", 
                  "WgtGroundRope", "TowDir", "GroundSpeed", "SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir", "BotCurSpeed", "WindDir",
                  "WindSpeed", "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal", "ThermoCline", "ThClineDepth", "CodendMesh",
                  "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelSampType", "MinTrawlDepth", "MaxTrawlDepth", sep = ",")
  
    cat(hhheadline, file = op, sep = "\n", append = TRUE)
    rm(hhheadline)
    
    ca_info <- populateHH(cruiseInfo, myVessel, chronData, op)

    hlheadline <- paste("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "SpecCodeType",
                        "SpecCode", "SpecVal", "Sex", "TotalNo", "CatIdentifier", "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode",
                        "LngtClass", "HLNoAtLngt", "DevStage", "LenMeasType", sep = ",")
  
    cat(hlheadline, file = op, sep = "\n", append = TRUE)
    rm(hlheadline)
    
    populateHLmeas(cruiseInfo, myVessel, chronData, cruiseCode, op)

    populateHLnonMeas(cruiseInfo, myVessel, chronData, cruiseCode, op)

    caheadline <- paste("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "SpecCodeType",
                        "SpecCode", "AreaType", "AreaCode", "LngtCode", "LngtClass", "Sex", "Maturity", "PlusGr", "AgeRings", "CANoAtLngt",
                        "IndWgt", "MaturityScale", "FishID", "GenSamp", "StomSamp", "AgeSource", "AgePrepMet", "OtGrading", "ParSamp", sep = ",")
  
    cat(caheadline, file = op, sep = "\n", append = TRUE)
    rm(caheadline)
    
    populateCAcore(cruiseInfo, myVessel, ca_info, cruiseCode, cruiseSeries, op)

    populateCAnoncore(cruiseInfo, myVessel, ca_info, cruiseCode, cruiseSeries, op)

    close(op)

    rm(list = c("cruiseInfo", "vesselName", "myVessel", "chronData"))

    if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCode & cruiseTable$fldSeriesName == cruiseSeries] == "N") {
      message("Warning: The requested cruise has not been closed and therefore has received no quality checks")
    }

  }

  # if data frame of cruises
  if(codeSwitch == 1) {

    cruiseTable <- sqlFetch(channel,"dbo.tblReferenceCruises")

    notClosedCruises <- NULL

    for (i in 1:nrow(cruiseCodeSeries)) {

      cruiseInfo <- getCruiseInfo(cruiseCodeSeries$fldSeriesName[i])

      vesselName <- sqlQuery(channel, vessel_qry(cruiseCodeSeries$fldCruiseName[i]))
      myVessel <- gsub("\\s", "", vesselName[,1])

      chronData <- sqlQuery(channel, chron_qry(cruiseCodeSeries$fldCruiseName[i]))

      op <- file(paste(path, cruiseCodeSeries$fldCruiseName[i], ".txt", sep = ""), "w")
      
          hhheadline <- paste("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "Month",
                  "Day", "TimeShot", "DepthStratum", "HaulDur", "DayNight", "ShootLat", "ShootLong", "HaulLat", "HaulLong", "StatRec", "Depth",
                  "HaulVal", "HydroStNo", "StdSpecRecCode", "BySpecRecCode", "DataType", "Netopening", "Rigging", "Tickler", "Distance", 
                  "Warplngt", "Warpdia", "WarpDen", "DoorSurface", "DoorWgt", "DoorSpread", "WingSpread", "Buoyancy", "KiteDim", 
                  "WgtGroundRope", "TowDir", "GroundSpeed", "SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir", "BotCurSpeed", "WindDir",
                  "WindSpeed", "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal", "ThermoCline", "ThClineDepth", "CodendMesh",
                  "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelSampType", "MinTrawlDepth", "MaxTrawlDepth", sep = ",")
  
      cat(hhheadline, file = op, sep = "\n", append = TRUE)
      rm(hhheadline)

      ca_info <- populateHH(cruiseInfo, myVessel, chronData, op)

      hlheadline <- paste("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "SpecCodeType",
                        "SpecCode", "SpecVal", "Sex", "TotalNo", "CatIdentifier", "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode",
                        "LngtClass", "HLNoAtLngt", "DevStage", "LenMeasType", sep = ",")
  
      cat(hlheadline, file = op, sep = "\n", append = TRUE)
      rm(hlheadline)
      
      populateHLmeas(cruiseInfo, myVessel, chronData, cruiseCodeSeries$fldCruiseName[i], op)

      populateHLnonMeas(cruiseInfo, myVessel, chronData, cruiseCodeSeries$fldCruiseName[i], op)
      
      caheadline <- paste("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "SpecCodeType",
                        "SpecCode", "AreaType", "AreaCode", "LngtCode", "LngtClass", "Sex", "Maturity", "PlusGr", "AgeRings", "CANoAtLngt",
                        "IndWgt", "MaturityScale", "FishID", "GenSamp", "StomSamp", "AgeSource", "AgePrepMet", "OtGrading", "ParSamp", sep = ",")
  
      cat(caheadline, file = op, sep = "\n", append = TRUE)
      rm(caheadline)

      populateCAcore(cruiseInfo, myVessel, ca_info, cruiseCodeSeries$fldCruiseName[i], cruiseCodeSeries$fldSeriesName[i], op)

      populateCAnoncore(cruiseInfo, myVessel, ca_info, cruiseCodeSeries$fldCruiseName[i], cruiseCodeSeries$fldSeriesName[i], op)

      close(op)

      rm(list = c("cruiseInfo", "vesselName", "myVessel", "chronData"))

      if(cruiseTable$fldCruiseClosed[cruiseTable$fldCruiseName == cruiseCodeSeries$fldCruiseName[i] & cruiseTable$fldSeriesName == cruiseCodeSeries$fldSeriesName[i]] == "N") {
        notClosedCruises <- c(notClosedCruises, cruiseCodeSeries$fldCruiseName[i])
      }

    }

    notClosedAsString <- paste(notClosedCruises, collapse = " ")
    warningMessage <- paste("Warning: The following requested cruises have not been closed and therefore have received no quality checks:", notClosedAsString, sep = " ")

    if(! is.null(notClosedCruises)) {message(warningMessage)}

  }

}
