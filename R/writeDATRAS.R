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

    uniGears <- unique(chronData$DATRASGear)

    for(j in 1:length(uniGears)) {

    op <- file(paste(path, cruiseCode, "_", uniGears[j], ".txt", sep = ""), "w")

    # Names changed 13/10/2025
    hhheadline <- paste("RecordHeader", "Quarter", "Country", "Platform", "Gear", "SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "Month",
                  "Day", "StartTime", "DepthStratum", "HaulDuration", "DayNight", "ShootLatitude", "ShootLongitude", "HaulLatitude", "HaulLongitude", "StatisticalRectangle", "BottomDepth",
                  "HaulValidity", "HydrographicStationID", "StandardSpeciesCode", "BycatchSpeciesCode", "DataType", "Netopening", "Rigging", "Tickler", "Distance", 
                  "WarpLength", "WarpDiameter", "WarpDensity", "DoorSurface", "DoorWeight", "DoorSpread", "WingSpread", "Buoyancy", "KiteArea", 
                  "GroundRopeWeight", "TowDirection", "SpeedGround", "SpeedWater", "SurfaceCurrentDirection", "SurfaceCurrentSpeed", "BottomCurrentDirection", "BottomCurrentSpeed", "WindDirection",
                  "WindSpeed", "SwellDirection", "SwellHeight", "SurfaceTemperature", "BottomTemperature", "SurfaceSalinity", "BottomSalinity", "ThermoCline", "ThermoClineDepth", "CodendMesh",
                  "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelagicSamplingType", "MinTrawlDepth", "MaxTrawlDepth", "SurveyIndexArea", "Survey", "EDMO", sep = ",")
  
    cat(hhheadline, file = op, sep = "\n", append = TRUE)
    rm(hhheadline)
    
    ca_info <- populateHH(cruiseInfo, cruiseSeries, myVessel, chronData[chronData$DATRASGear == uniGears[j],], op)

    hlheadline <- paste("RecordHeader", "Quarter", "Country", "Platform", "Gear", "SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "SpeciesCodeType",
                        "SpeciesCode", "SpeciesValidity", "SpeciesSex", "TotalNumber", "SpeciesCategory", "SubsampledNumber", "SubsamplingFactor", "SubsampleWeight", "SpeciesCategoryWeight", "LengthCode",
                        "LengthClass", "NumberAtlength", "DevelopmentStage", "LengthType", "Survey", sep = ",")
  
    cat(hlheadline, file = op, sep = "\n", append = TRUE)
    rm(hlheadline)
    
    populateHLmeas(cruiseInfo, myVessel, chronData[chronData$DATRASGear == uniGears[j],], cruiseCode, cruiseSeries, op)

    populateHLnonMeas(cruiseInfo, myVessel, chronData[chronData$DATRASGear == uniGears[j],], cruiseCode, cruiseSeries, op)

    caheadline <- paste("RecordHeader", "Quarter", "Country", "Platform", "Gear", "SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "SpeciesCodeType",
                        "SpeciesCode", "AreaType", "AreaCode", "LengthCode", "LengthClass", "IndividualSex", "IndividualMaturity", "AgePlusGroup", "IndividualAge", "NumberAtLength",
                        "IndividualWeight", "MaturityScale", "FishID", "GeneticSamplingFlag", "StomachSamplingFlag", "AgeSource", "AgePreparationMethod", "OtolithGrading", "ParasiteSamplingFlag",
                        "LiverWeight", "Survey", sep = ",")
  
    cat(caheadline, file = op, sep = "\n", append = TRUE)
    rm(caheadline)
    
    populateCAcore(cruiseInfo, myVessel, ca_info, cruiseCode, cruiseSeries, uniGears[j], op)

    populateCAnoncore(cruiseInfo, myVessel, ca_info, cruiseCode, cruiseSeries, uniGears[j], op)

    close(op)

    }

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

      uniGears <- unique(chronData$DATRASGear)

      for(j in 1:length(uniGears)) {

      op <- file(paste(path, cruiseCodeSeries$fldCruiseName[i], "_", uniGears[j], ".txt", sep = ""), "w")
      
    hhheadline <- paste("RecordHeader", "Quarter", "Country", "Platform", "Gear", "SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "Month",
                  "Day", "StartTime", "DepthStratum", "HaulDuration", "DayNight", "ShootLatitude", "ShootLongitude", "HaulLatitude", "HaulLongitude", "StatisticalRectangle", "BottomDepth",
                  "HaulValidity", "HydrographicStationID", "StandardSpeciesCode", "BycatchSpeciesCode", "DataType", "Netopening", "Rigging", "Tickler", "Distance", 
                  "WarpLength", "WarpDiameter", "WarpDensity", "DoorSurface", "DoorWeight", "DoorSpread", "WingSpread", "Buoyancy", "KiteArea", 
                  "GroundRopeWeight", "TowDirection", "SpeedGround", "SpeedWater", "SurfaceCurrentDirection", "SurfaceCurrentSpeed", "BottomCurrentDirection", "BottomCurrentSpeed", "WindDirection",
                  "WindSpeed", "SwellDirection", "SwellHeight", "SurfaceTemperature", "BottomTemperature", "SurfaceSalinity", "BottomSalinity", "ThermoCline", "ThermoClineDepth", "CodendMesh",
                  "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelagicSamplingType", "MinTrawlDepth", "MaxTrawlDepth", sep = ",")
  
      cat(hhheadline, file = op, sep = "\n", append = TRUE)
      rm(hhheadline)

      ca_info <- populateHH(cruiseInfo, cruiseCodeSeries$fldSeriesName[i], myVessel, chronData[chronData$DATRASGear == uniGears[j],], op)

    hlheadline <- paste("RecordHeader", "Quarter", "Country", "Platform", "Gear", "SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "SpeciesCodeType",
                        "SpeciesCode", "SpeciesValidity", "SpeciesSex", "TotalNumber", "SpeciesCategory", "SubsampledNumber", "SubsamplingFactor", "SubsampleWeight", "SpeciesCategoryWeight", "LengthCode",
                        "LengthClass", "NumberAtlength", "DevelopmentStage", "LengthType", sep = ",")
  
      cat(hlheadline, file = op, sep = "\n", append = TRUE)
      rm(hlheadline)
      
      populateHLmeas(cruiseInfo, myVessel, chronData[chronData$DATRASGear == uniGears[j],], cruiseCodeSeries$fldCruiseName[i], cruiseCodeSeries$fldSeriesName[i], op)

      populateHLnonMeas(cruiseInfo, myVessel, chronData[chronData$DATRASGear == uniGears[j],], cruiseCodeSeries$fldCruiseName[i], cruiseCodeSeries$fldSeriesName[i], op)
      
    caheadline <- paste("RecordHeader", "Quarter", "Country", "Platform", "Gear", "SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "SpeciesCodeType",
                        "SpeciesCode", "AreaType", "AreaCode", "LengthCode", "LengthClass", "IndividualSex", "IndividualMaturity", "AgePlusGroup", "IndividualAge", "NumberAtLength",
                        "IndividualWeight", "MaturityScale", "FishID", "GeneticSamplingFlag", "StomachSamplingFlag", "AgeSource", "AgePreparationMethod", "OtolithGrading", "ParasiteSamplingFlag", sep = ",")
  
      cat(caheadline, file = op, sep = "\n", append = TRUE)
      rm(caheadline)

      populateCAcore(cruiseInfo, myVessel, ca_info, cruiseCodeSeries$fldCruiseName[i], cruiseCodeSeries$fldSeriesName[i], uniGears[j], op)

      populateCAnoncore(cruiseInfo, myVessel, ca_info, cruiseCodeSeries$fldCruiseName[i], cruiseCodeSeries$fldSeriesName[i], uniGears[j], op)

      close(op)

      }

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








