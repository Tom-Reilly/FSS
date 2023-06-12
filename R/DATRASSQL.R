#DATRAS SQL

vessel_qry <- function(cruiseCode) {

  vessel_qry <- paste("SELECT dbo.tblReferenceShips.fldDatrasShipCode FROM dbo.tblReferenceShips INNER JOIN dbo.tblReferenceCruises 
                    ON(dbo.tblReferenceShips.fldShipTag=dbo.tblReferenceCruises.fldShipTag) WHERE dbo.tblReferenceCruises.fldCruiseName='", cruiseCode, "'", sep = "")
  
  return(vessel_qry <- gsub('\n', '', vessel_qry))
  
}

chron_qry <- function(cruiseCode) {

  chron_qry <- paste("
	SELECT 
	dbo.tblDataStationLogs.fldCruiseName AS Cruise, 
	LEFT(dbo.tblDataStationLogs.fldICESRectangle,2) AS StatSquare1,
	RIGHT(dbo.tblDataStationLogs.fldICESRectangle,2) AS StatSquare2,
	dbo.tblDataStationLogs.fldCruiseStationNumber As Haul,
	dbo.tblDataStationLogs.fldDateTimeShot AS TimeShot,
	dbo.tblDataStationLogs.fldDateTimeHaul AS TimeHaul,
	dbo.tblDataStationLogs.fldShotLatDecimalDegrees AS ShotLat,
	dbo.tblDataStationLogs.fldShotLonDecimalDegrees AS ShotLon,
	dbo.tblDataStationLogs.fldShotShipsHeading AS TowDir,
	dbo.tblDataStationLogs.fldShotDepth AS ShotDepth,
	dbo.tblDataStationLogs.fldHaulLatDecimalDegrees AS HaulLat,
	dbo.tblDataStationLogs.fldHaulLonDecimalDegrees AS HaulLon,
	dbo.tblDataStationLogs.fldHaulDepth AS HaulDepth,
	dbo.tblDataStationLogs.fldTowDuration AS Duration,
	dbo.tblDataStationLogs.fldAssociatedHydro AS HydroStn,
	dbo.tblDataStationLogs.fldTideDirection AS TideDir,
	dbo.tblDataStationLogs.fldTideSpeed AS TideSpeed,
	dbo.tblDataStationLogs.fldWindDirection AS WindDirection,
	dbo.tblDataStationLogs.fldWindSpeed AS WindSpeed,
	dbo.tblDataStationLogs.fldSurfaceTemp AS SurfaceTemperature,
	dbo.tblDataStationLogs.fldSurfaceSalinity AS SurfaceSalinity,
	dbo.tblDataStationLogs.fldBottomTemp AS BottomTemperature,
	dbo.tblDataStationLogs.fldBottomSalinity AS BottomSalinity,
	dbo.tblDataStationLogs.fldSwellHeight AS SwellHeight,
	dbo.tblDataStationLogs.fldStratum AS Stratum,
	dbo.tblDataStationLogs.fldSwellDirection AS SwellDirection,
	dbo.tblDataGearDeployments.fldGearCode AS GearCode,
	dbo.tblDataGearDeployments.fldValidityCode AS Valid,
	dbo.tblDataGearDeployments.fldGearAdditional1 AS WarpOut,
	dbo.tblDataGearDeployments.fldDoorSpread AS DoorSpread,
	dbo.tblDataGearDeployments.fldWingSpread AS WingSpread,
	dbo.tblDataGearDeployments.fldGearAdditional2 AS TowDist,
	dbo.tblDataGearDeployments.fldGearAdditional3 AS SOG,
	dbo.tblDataGearDeployments.fldGearAdditional4 AS DayNight,
	dbo.tblDataGearDeployments.fldHeadlineHeight AS HeadLine
	 FROM dbo.tblDataStationLogs INNER JOIN dbo.tblDataGearDeployments 
	 ON(dbo.tblDataStationLogs.fldCruiseName=dbo.tblDataGearDeployments.fldCruisename AND dbo.tblDataStationLogs.fldCruiseStationNumber=dbo.tblDataGearDeployments.fldCruiseStationNumber)
	 WHERE dbo.tblDataStationLogs.fldCruiseName='" , cruiseCode, "' ORDER BY dbo.tblDataStationLogs.fldCruiseStationNumber", sep = "")

chron_qry <- gsub('\n', '', chron_qry)

}
# 20210630 added category and raising factor to query and changed raised number to measured number, removed sum and group by
length_qry <- function(cruiseCode, chronData) {
  # 20230612 added join to pull through the alternate species code
  length_sql <- paste("
	SELECT 
	dbo.tblDataLengthSamples.fldMainSpeciesCode AS SpCode,
	dbo.tblReferenceMainSpecies.fldAlternateSpeciesCode,
	dbo.tblDataLengthSamples.fldSex AS Sex,
	dbo.tblDataLengthSamples.fldLengthGroup AS Length,
	dbo.tblDataLengthSamples.fldCategoryNumber AS Category,
	dbo.tblDataLengthSamples.fldLengthGroupRaisingFactor AS RaisingFactor,
	dbo.tblDataLengthSamples.fldCategoryRaisedNumberAtLength AS Raised,
	dbo.tblDataLengthSamples.fldMeasuredNumberAtLength AS Measured 
	FROM 	dbo.tblDataLengthSamples 
	LEFT JOIN dbo.tblReferenceMainSpecies ON dbo.tblDataLengthSamples.fldMainSpeciesCode = dbo.tblReferenceMainSpecies.fldMainSpeciesCode
	WHERE 
	dbo.tblDataLengthSamples.fldCruiseName='",cruiseCode,"' AND 
	dbo.tblDataLengthSamples.fldCruiseStationNumber =", chronData$Haul, 
                      " AND fldGearCode=", chronData$GearCode, " ORDER BY  
	dbo.tblDataLengthSamples.fldCruiseName,
	dbo.tblDataLengthSamples.fldCruiseStationNumber,
	dbo.tblDataLengthSamples.fldMainSpeciesCode, 
	dbo.tblDataLengthSamples.fldSex, 
	dbo.tblDataLengthSamples.fldLengthGroup,
	dbo.tblDataLengthSamples.fldCategoryNumber,
	dbo.tblDataLengthSamples.fldLengthGroupRaisingFactor,
	dbo.tblDataLengthSamples.fldCategoryRaisedNumberAtLength,
	dbo.tblDataLengthSamples.fldMeasuredNumberAtLength", sep = "")
  
  length_qry <- gsub('\n', '', length_sql)
}

co_qry <- function(cruiseCode, chronData) {
  # 20230612 added join to pull through the alternate species code
  co_sql = paste( "SELECT fldMainSpeciesCode AS Species,
	dbo.tblReferenceMainSpecies.fldAlternateSpeciesCode,
	fldSex AS Sex,
	fldGearCode AS GearCode,
	fldCruiseStationNumber AS Haul,
	SUM(fldCatchNumber) AS Count, SUM(fldCatchWeight) AS Weight 
	FROM dbo.tblDataCategories 
	LEFT JOIN dbo.tblReferenceMainSpecies ON dbo.tblDataLengthSamples.fldMainSpeciesCode = dbo.tblReferenceMainSpecies.fldMainSpeciesCode
	WHERE fldCruiseName='", cruiseCode, "' AND fldCruiseStationNumber =", 
                  chronData$Haul, " AND fldGearCode =", chronData$GearCode," AND fldMeasuringInterval is null GROUP BY fldCruiseName, fldCruiseStationNumber, fldGearCode, fldMainSpeciesCode, fldSex", 
                  sep = "")
  
  co_qry <- gsub("\\n", "", co_sql)
}

caCore_qry <- function(cruiseCode) {
  
  ca_sql <- paste("
		SELECT 
		a.fldCruiseStationNumber AS Haul,
		a.fldGearCode AS GearCode,
		a.fldMainSpeciesCode AS SpCode, 
		a.fldFishLength AS Lngth,
		a.fldFishSex as Sex, 
    		a.fldFishMaturity as Maturity,
		a.fldResult1 AS Age,
		AVG(a.fldFishWholeWeight) AS Weight,
    		COUNT(a.fldInternalBiologicalSampleID) AS CaNoAtLen  
		FROM dbo.tblDataBiologicalSamples a INNER JOIN dbo.tblDataGearDeployments b ON(a.fldCruiseName=b.fldCruisename AND a.fldCruiseStationNumber=b.fldCruiseStationNumber)
  		WHERE a.fldCruiseName='", cruiseCode, "' AND a.fldResult1 is not null AND b.fldValidityCode='V' GROUP BY a.fldCruiseStationNumber, a.fldGearCode, a.fldMainSpeciesCode,  a.fldFishLength, a.fldFishSex, a.fldFishMaturity, a.fldResult1; ", sep = "") 
  ca_sql <- gsub('\n','',ca_sql)	
  caCore_qry <- gsub('\t','',ca_sql)  
  
}

caNonCore_qry <- function(cruiseCode) {
  
  ca_sql <- paste("
		SELECT 
		a.fldCruiseStationNumber AS Haul,
		a.fldGearCode AS GearCode,
		a.fldMainSpeciesCode AS SpCode, 
		a.fldFishLength AS Lngth,
		a.fldFishSex as Sex, 
    		a.fldFishMaturity as Maturity,
		a.fldResult1 AS Age,
		AVG(a.fldFishWholeWeight) AS Weight,
    		COUNT(a.fldInternalBiologicalSampleID) AS CaNoAtLen  
		FROM dbo.tblDataBiologicalSamples a INNER JOIN dbo.tblDataGearDeployments b ON(a.fldCruiseName=b.fldCruisename AND a.fldCruiseStationNumber=b.fldCruiseStationNumber)
  		WHERE a.fldCruiseName='", cruiseCode, "' AND b.fldValidityCode='V' GROUP BY a.fldCruiseStationNumber, a.fldGearCode, a.fldMainSpeciesCode,  a.fldFishLength, a.fldFishSex, a.fldFishMaturity, a.fldResult1; ", sep = "") # removed  AND fldResult1 is not null
  
  ca_sql <- gsub('\n','',ca_sql)	
  caNonCore_qry <- gsub('\t','',ca_sql)  
  
}
