#DATRAS Functions

getCruiseInfo <- function(CruiseSeries) {

  # Temporarily extract the table that houses the series that have associated strata
  temp <- sqlFetch(channel, "dbo.tblSeriesStratum")

  SurveyQuarter <- as.numeric(gsub("[^0-9]", "", CruiseSeries))
  #if(substr(CruiseSeries,1,6) == "WCIBTS" & SurveyQuarter == 1) {SurveyAreaType <- 21
  #} else {
  #  if(substr(CruiseSeries,1,6) == "WCIBTS" & SurveyQuarter == 4) {SurveyAreaType <- 22
  #  } else {
  #    SurveyAreaType <- 0
  #  }
  #}

  SurveyAreaType <- 0

  if(any(grepl(CruiseSeries, temp[["fldSeriesName"]]))) {HHStratum <- 1} else {HHStratum <- 0}

  rm(temp)

  cruiseInfo <- list(SurveyQuarter = SurveyQuarter, SurveyAreaType = SurveyAreaType, HHStratum = HHStratum)

  return(cruiseInfo)
}

# 02/11/2017 - Changed the survey area type to be 0 for all cruises as the area code is now always set to be stat sq rather than stratum
