# Populate DATRAS

populateHH <- function(cruiseInfo, myVessel, chronData, file) {

  ca_info <- data.frame()
  
  for(i in 1:nrow(chronData)) {

    hhline <- paste("HH") #F1 - RecordType
    hhline <- paste(hhline, cruiseInfo[["SurveyQuarter"]], sep = ",") #F2 - Quarter
    hhline <- paste(hhline, "GB-SCT", sep = ",") #F3 - Country
    # Changed country code from "SCO" on 03/11/2020

    hhline <- paste(hhline, "748S", sep = ",") #F4 - Vessel
    # Changed vessel from output of myVessel to "748S"

    gear_qry <- paste("SELECT * FROM dbo.tblReferenceMainGearCodes WHERE fldGearCode=", chronData$GearCode[i], sep="")
    gear_detail <- sqlQuery(channel, gear_qry)

    hhline <- paste(hhline, gear_detail$fldDATRASCode[1], sep = ",") #F5 Gear
    hhline <- paste(hhline, gear_detail$fldDATRASSweepLength[1], sep = ",") #F6 - Sweeplength
    hhline <- paste(hhline, gear_detail$fldDATRASGearException, sep = ",") #F7 - GearExp
    hhline <- paste(hhline, gear_detail$fldDATRASDoorType, sep = ",") #F8 - DoorType

    hhline <- paste(hhline, chronData$Haul[i], sep = ",") #F9 - StnNo
    hhline <- paste(hhline, chronData$Haul[i], sep = ",") #F10 - Haul
    hhline <- paste(hhline, as.numeric(format(chronData$TimeShot[i], "%Y")), sep = ",") #F11 - Year

    ca_haul <- chronData$Haul[i]
    ca_year <- as.numeric(format(chronData$TimeShot[i], "%Y")) #Slight cheat - determine year here and use in ca records rather than doing a lot of lookups
    ca_info <- rbind(ca_info, c(ca_haul,ca_year))

    m <- as.numeric(format(chronData$TimeShot[i], "%m"))
    if (m < 10) {
      m <- paste("0", m, sep = "")
    }

    hhline <- paste(hhline, m, sep = ",") #F12 - Month

    d <- as.numeric(format(chronData$TimeShot[i], "%d"))
    if (d < 10) {
      d <- paste("0", d, sep = "")
    }

    hhline <- paste(hhline, d, sep = ",") #F13 - day

    tshoth <- as.numeric(format(chronData$TimeShot[i], "%H"))

    if(tshoth < 10) {tshoth <- paste("0", tshoth, sep = "") }

    tshotm <- as.numeric(format(chronData$TimeShot[i], "%M"))

    if(tshotm < 10) {tshotm <- paste("0", tshotm, sep = "") }

    tshot <- paste(tshoth, tshotm, sep = "")

    hhline <- paste(hhline, tshot, sep = ",") #F14 - time shot

    #Ignore Strata on HH record. Strata for WC IBTS on CA records.

    if(cruiseInfo[["HHStratum"]] == 0) {
      strat = -9
    } else {
      strat = chronData$Stratum[i]
    }

    hhline <- paste(hhline, strat, sep = ",") #F15 - Stratum

    hhline <- paste(hhline, chronData$Duration[i], sep = ",") #F16 - Duration

    if(is.na(chronData$DayNight[i])) {dn <- "D"} else {dn <- chronData$DayNight[i]}

    hhline <- paste(hhline, dn, sep = ",") #F17 - Day/Night

    shlat <- format(round(chronData$ShotLat[i], 4), nsmall = 4)
    shlon <- format(round(chronData$ShotLon[i], 4), nsmall = 4)
    halat <- format(round(chronData$HaulLat[i], 4), nsmall = 4)
    halon <- format(round(chronData$HaulLon[i], 4), nsmall = 4)

    hhline <- paste(hhline, shlat, sep = ",") #F18 - Shootlat
    hhline <- paste(hhline, shlon, sep = ",") #F19 - Shootlon
    hhline <- paste(hhline, halat, sep = ",") #F20 - Haullat
    hhline <- paste(hhline, halon, sep = ",") #F21 - HaulLon

    statsquare <- paste(chronData$StatSquare1[i], chronData$StatSquare2[i], sep = "")
    hhline <- paste(hhline, statsquare, sep = ",") #F22 - stat square

    hhline <- paste(hhline, round(chronData$ShotDepth[i], digits = 0), sep = ",") #F23 - depth

    hhline <- paste(hhline, chronData$Valid[i], sep = ",") #F24 - Haul Val

    #CHECK for standardised formatting (e.g. previous example: 74SC0258)

    if (is.na(chronData$HydroStn[i])) {hydro <- -9 } else {hydro <- paste("74SC", sprintf("%04d", chronData$HydroStn[i]), sep = "")}

    hhline <- paste(hhline, hydro, sep = ",") #F25 - Associated Hydro

    # 20210630 data type changed to P
    if(chronData$Valid[i] == 'V') {
      hhline <- paste(hhline, 1,sep = ",") #F26 - Species Reporting
      hhline <- paste(hhline, 1,sep = ",") #F27 - BycatchSpecies Reporting
      hhline <- paste(hhline, "P",sep = ",") #F28 - DataType
    } else {
      hhline <- paste(hhline, 0, sep = ",") #F26 - Species Reporting
      hhline <- paste(hhline, 0,sep = ",") #F27 - BycatchSpecies Reporting
      hhline <- paste(hhline, "P", sep = ",") #F28 - DataType
    }

    if(is.na(chronData$HeadLine[i])) {hdl <- -9} else {hdl <- format(round(chronData$HeadLine[i], 1), nsmall = 1)}

    hhline <- paste(hhline, hdl, sep = ",") #F29 - Netopening

    #Back to the gear detail query to pull reference on these

    hhline <- paste(hhline, gear_detail$fldDATRASRigging[1], sep = ",") #F30 - Rigging

    hhline <- paste(hhline, gear_detail$fldDATRASTicklers[1], sep = ",") #F31 - Tickler

    if(is.na(chronData$TowDist[i])) { tdist <- -9} else {tdist <- round(chronData$TowDist[i], digits = 0)}

    hhline <- paste(hhline, tdist, sep = ",") #F32 - Distance

    if(is.na(chronData$WarpOut[i])) { wop <- -9 } else { wop <- round(chronData$WarpOut[i], digits = 0)}

    hhline <- paste(hhline, wop, sep = ",") #F33 - Warplength

    hhline <- paste(hhline, gear_detail$fldDATRASWarpDia[1], sep = ",") #F34 - Warpdia
    hhline <- paste(hhline, gear_detail$fldDATRASWarpDen[1], sep = ",") #F35 - WarpDen
    hhline <- paste(hhline, gear_detail$fldDATRASDoorSurface[1], sep = ",") #F36 - DoorSurf
    hhline <- paste(hhline, gear_detail$fldDATRASDoorWgt[1], sep = ",") #F37 - DoorWgt

    if(is.na(chronData$DoorSpread[i])) {dsp <- -9} else {dsp <- round(chronData$DoorSpread[i], digits = 0)}

    hhline <- paste(hhline, dsp, sep = ",") #F38 - Door Spread

    if(is.na(chronData$WingSpread[i])) {wsp <- -9} else {wsp <- round(chronData$WingSpread[i], digits = 0)}

    hhline <- paste(hhline, wsp, sep = ",") #F39 - Wing Spread

    hhline <- paste(hhline, gear_detail$fldDATRASBuoyancy[1], sep = ",") #F40 - Buoyancy
    hhline <- paste(hhline, gear_detail$fldDATRASKiteDim[1], sep = ",") #F41 - Kite Dim
    hhline <- paste(hhline, gear_detail$fldDATRASWgtGroundRope[1], sep = ",") #F42 - Wgt GroundRope

    if(is.na(chronData$TowDir[i])) {tdir <- -9} else {tdir <- chronData$TowDir[i]}

    hhline <- paste(hhline, tdir, sep = ",") #F43 - Tow Direction

    if(is.na(chronData$SOG[i])) {SOG <- -9} else {SOG <- format(round(chronData$SOG[i], 1), nsmall = 1)}

    hhline <- paste(hhline, SOG, sep = ",") #F44 - Ground speed

    hhline <- paste(hhline, -9, sep = ",") #F45 - Speed through water (NOT RECORDED)

    if(is.na(chronData$TideDir[i])) {tidedir <- -9} else {tidedir <- chronData$TideDir[i]}

    hhline <- paste(hhline, tidedir, sep = ",") #F46 - Surface current direction

    if(is.na(chronData$TideSpeed[i])) {tidesp <- -9} else {
      #Convert to m/s
      tsp <- 0.5144 * chronData$TideSpeed[i]
      tidesp <- format(round(tsp, 1), nsmall = 1)
    }

    hhline <- paste(hhline, tidesp, sep = ",") #F47 - Surface current speed

    hhline <- paste(hhline, -9,sep = ",") #F48 - Bottom current direction (NOT RECORDED)
    hhline <- paste(hhline, -9,sep = ",") #F49 - Bottom current speed (NOT RECORDED)

    if(is.na(chronData$WindDirection[i])) {widir <- -9} else {widir <- chronData$WindDirection[i]}

    hhline <- paste(hhline, widir, sep = ",") #F50 - Wind direction

    if(is.na(chronData$WindSpeed[i])) {wisp <- -9} else {
      #Convert to m/s
      wsp <- 0.5144 * chronData$WindSpeed[i]
      wisp <- round(wsp, digits = 0)
    }

      hhline <- paste(hhline, wisp, sep = ",") #F51 - Wind Speed

      if(is.na(chronData$SwellDirection[i])) {swdir <- -9} else {swdir <- chronData$SwellDirection[i]}

      hhline <- paste(hhline, swdir, sep = ",") #F52 - Swell Direction

      if(is.na(chronData$SwellHeight[i])) {swdhg <- -9} else {swhg <- format(round(chronData$SwellHeight[i], 1), nsmall = 1)}

      hhline <- paste(hhline, swhg, sep = ",") #F53 - Swell Height

      if(is.na(chronData$SurfaceTemperature[i])) {stemp <- -9} else {stemp <- format(round(chronData$SurfaceTemperature[i], 1), nsmall = 1)}

      hhline <- paste(hhline, stemp, sep = ",") #F54 - Surface Temperature

      if(is.na(chronData$BottomTemperature[i])) {btemp <- -9} else {btemp <- format(round(chronData$BottomTemperature[i], 1), nsmall = 1)}

      hhline <- paste(hhline, btemp, sep = ",") #F55 - Bottom Temperature

      if(is.na(chronData$SurfaceSalinity[i])) {ssal <- -9} else {ssal <- format(round(chronData$SurfaceSalinity[i], 3), nsmall = 3)}

      hhline <- paste(hhline, ssal, sep = ",") #F56 - Surface salinity

      if(is.na(chronData$BottomSalinity[i])) {bsal <- -9} else {bsal <- format(round(chronData$BottomSalinity[i], 3), nsmall = 3)}

      hhline <- paste(hhline, bsal, sep = ",") #F57 - Surface salinity

      hhline <- paste(hhline, -9, sep = ",") #F58 - Thermocline (NOT RECORDED)
      hhline <- paste(hhline, -9, sep = ",") #F59 - Thermocline Depth (NOT RECORDED)
    
      hhline <- paste(hhline, -9, sep = ",") #F60 - CodendMesh (codend mesh size in mm)
    
      hhline <- paste(hhline, -9, sep = ",") #F61 - SecchiDepth (secchi depth in metres)
    
      hhline <- paste(hhline, -9, sep = ",") #F62 - Turbidity (turbidity in NTU)
    
      hhline <- paste(hhline, -9, sep = ",") #F63 - TidePhase (tidal phase in minutes before the next high tide)
      hhline <- paste(hhline, -9, sep = ",") #F64 - TideSpeed (Speed of tide in m/s)
    
      hhline <- paste(hhline, -9, sep = ",") #F65 - PelSampType (pelagic trawl sampling type)
    
      hhline <- paste(hhline, -9, sep = ",") #F66 - MinTrawlDepth (highest point of the pelagic trawling)
      hhline <- paste(hhline, -9, sep = ",") #F67 - MaxTrawlDepth (lowest point of the pelagic trawling)

      cat(hhline, file = file, sep = "\n", append = TRUE)
      rm(hhline)

  }

  return(ca_info)

}


populateHLmeas <- function(cruiseInfo, myVessel, chronData, cruiseCode, file) {

  for(i in 1:nrow(chronData)) {

    if(chronData$Valid[i] == 'V') {

      length_data <- sqlQuery(channel, length_qry(cruiseCode, chronData[i, ]))
      #20230612 added stipulation to remove species with no Aphia ID
      length_data = length_data %>%
               filter(! is.na(fldAlternateSpeciesCode)) %>%
               group_by(SpCode,Category) %>%
               mutate(id=paste(Category,cumsum(!duplicated(RaisingFactor)),sep=""))

      for(s in 1:nrow(length_data)) {

        if(length_data$Measured[s]>=1) {

          hlline <- paste("HL", sep = ",") #F1 Record Type
          hlline <- paste(hlline, cruiseInfo[["SurveyQuarter"]], sep = ",") #F2 Quarter
          hlline <- paste(hlline, "GB-SCT", sep = ",") #F3 Country
          hlline <- paste(hlline, "748S", sep = ",") #F4 Ship

          gear_qry <- paste("SELECT * FROM dbo.tblReferenceMainGearCodes WHERE fldGearCode=", chronData$GearCode[i], sep="")
          gear_detail <- sqlQuery(channel, gear_qry)

          hlline <- paste(hlline, gear_detail$fldDATRASCode[1], sep = ",") #F5 Gear
          hlline <- paste(hlline, gear_detail$fldDATRASSweepLength[1], sep = ",") #F6 - Sweeplength
          hlline <- paste(hlline, gear_detail$fldDATRASGearException[1], sep = ",") #F7 - GearExp
          hlline <- paste(hlline, gear_detail$fldDATRASDoorType[1], sep = ",") #F8 - DoorType

          hlline <- paste(hlline, chronData$Haul[i], sep = ",") #F9 - StnNo
          hlline <- paste(hlline, chronData$Haul[i], sep = ",") #F10 - Haul
          hlline <- paste(hlline, as.numeric(format(chronData$TimeShot[i], "%Y")), sep = ",") #F11 - Year

          hlline <- paste(hlline, "W", sep = ",") #F12 - Species code type for worms aphia id =w

          #Get AphiaID for species code
          aphia_sql = paste("SELECT TOP 1 fldAlternateSpeciesCode FROM dbo.tblReferenceMainSpecies WHERE fldMainSpeciesCode='", length_data$SpCode[s], "'", sep = "")
          aphia_data <- sqlQuery(channel, aphia_sql)
          aph <- aphia_data[1,1]
          AphiaID <- aph

          hlline <- paste(hlline, AphiaID, sep = ",") #F13 - Spcode
          hlline <- paste(hlline, 1, sep = ",") #F14 - SpecVal

          # 10/02/2020 added stipulation to change berried B to female F
          if(length_data$Sex[s] == 'U') {
            sx <- -9
          } else if(length_data$Sex[s] == 'B') {
            sx <- 'F'
          } else {
            sx <- length_data$Sex[s]
          }

          hlline <- paste(hlline, sx, sep = ",") #F15 - Sex

          sp <- length_data$SpCode[s]
          # 20210630 changed measured to raised and added stipulation for category
          tot_fish <- sum(length_data$Raised[which(length_data$SpCode == length_data$SpCode[s] & length_data$Sex == length_data$Sex[s] & length_data$Category == length_data$Category[s])])
          tot_fish <- round(tot_fish, 0)

          hlline <- paste(hlline, tot_fish, sep = ",") #F16 - Total number of fish

          # 20230613 Added stipulation to change category identifier where species has both berried devStage and no devStage females
          uniqSex = length_data %>%
            ungroup %>%
            filter(SpCode == SpCode[s],
                   Category == Category[s]) %>%
            select(Sex) %>%
            unique()

          if(length_data$Sex[s] == "B" & "F" %in% uniqSex) {
            catID = "21"
          } else {
            catID = length_data$id[s]
          }
          # 20210630 changed from 1 to populate with sub categories
          hlline <- paste(hlline, catID, sep = ",") #F17 - Category Identifier

          # Added stipulation for category
          noMeas <- sum(length_data$Measured[which(length_data$SpCode==length_data$SpCode[s] & length_data$Sex==length_data$Sex[s] & length_data$Category == length_data$Category[s])])
          hlline <- paste(hlline,noMeas,sep=",") #F18 - Number measured in haul
          #hlline <- paste(hlline, -9, sep = ",") #F18 - Number measured in haul - TEMPORARILY SET TO -9 DUE TO LACK OF RAISING FACTOR. ICES FORMAT IS SMALLINT, SO WILL NOT SUPPORT RAISED NUMBERS HERE.

          #NOTE this is where we are missing raising factors 20210630 changed to provide the raising factor
          hlline <- paste(hlline, sprintf("%.4f",length_data$RaisingFactor[s]), sep = ",") #F19 - Sub factor

          hlline <- paste(hlline, -9, sep = ",") #F20 - Sub weight

          # added stipualtion for category
          # 20230612 added in species sex to categorisation so catch weight is split by sex
          cw_sql <- paste("SELECT SUM(fldCatchWeight) AS CatchWeight FROM dbo.tblDataCategories WHERE fldCruiseName='", cruiseCode, "' AND fldCruiseStationNumber=", chronData$Haul[i], " AND fldGearCode=", chronData$GearCode[i], " AND fldMainSpeciesCode='", length_data$SpCode[s], "'", " AND fldSex='", length_data$Sex[s], "'", " AND fldCategoryNumber='", length_data$Category[s], "'",sep = "")
          catch_weight_data <- sqlQuery(channel, cw_sql)
          catch_weight <- round((catch_weight_data$CatchWeight[1] * 1000), 0)

          hlline <- paste(hlline, catch_weight, sep = ",") #F21 - Catch Weight

          hlline <- paste(hlline, ".", sep = ",") #F22 - Length class code

          hlline <- paste(hlline, length_data$Length[s], sep = ",") #F23 - Length class

          hlline <- paste(hlline, round(length_data$Measured[s], digits = 0), sep = ",") #F24 - Number measured at length
          
          # code added to populate new devstage field
          if(length_data$Sex[s] == 'B') {
            dev <- 'B'
          } else {
            dev <- -9
          }
          
          hlline <- paste(hlline, dev, sep = ",") #F25 - DevStage (Development or maturity stage defined by non-invasive obs)

          hlline <- paste(hlline, -9, sep = ",") #F26 LenMeasType (Type of length measurement)

          cat(hlline, file = file, sep = "\n", append = TRUE)
          rm(hlline)
        }

      }

    }

  }

}


populateHLnonMeas <- function(cruiseInfo, myVessel, chronData, cruiseCode, file) {

  for(i in 1:nrow(chronData)) {

    if(chronData$Valid[i] == 'V') {

      NotMeasuredSp <- sqlQuery(channel, co_qry(cruiseCode, chronData[i, ]))
      # 20230612 added stipulation to remove NA aphia ID
      NotMeasuredSp <- NotMeasuredSp %>% filter(! is.na(fldAlternateSpeciesCode))

      if(is.null(nrow(NotMeasuredSp)) == FALSE && (nrow(NotMeasuredSp) >= 1)) {
        for(s in 1:nrow(NotMeasuredSp)) {

          hlline <- paste("HL", sep = ",") #F1 Record Type
          hlline <- paste(hlline, cruiseInfo[["SurveyQuarter"]], sep = ",") #F2 Quarter
          hlline <- paste(hlline, "GB-SCT", sep = ",") #F3 Country
          hlline <- paste(hlline, "748S", sep = ",") #F4 Ship

          gear_qry <- paste("SELECT * FROM dbo.tblReferenceMainGearCodes WHERE fldGearCode=", chronData$GearCode[i], sep="")
          gear_detail <- sqlQuery(channel, gear_qry)

          hlline <- paste(hlline, gear_detail$fldDATRASCode[1], sep = ",") #F5 Gear
          hlline <- paste(hlline, gear_detail$fldDATRASSweepLength[1], sep = ",") #F6 - Sweeplength
          hlline <- paste(hlline, gear_detail$fldDATRASGearException[1], sep = ",") #F7 - GearExp
          hlline <- paste(hlline, gear_detail$fldDATRASDoorType[1], sep = ",") #F8 - DoorType

          hlline <- paste(hlline, chronData$Haul[i], sep = ",") #F9 - StnNo
          hlline <- paste(hlline, chronData$Haul[i], sep = ",") #F10 - Haul
          hlline <- paste(hlline, as.numeric(format(chronData$TimeShot[i], "%Y")), sep = ",") #F11 - Year

          hlline <- paste(hlline, "W", sep = ",") #F12 - Species code type for worms aphia id =w

          #Get AphiaID for species code
          aphia_sql = paste("SELECT TOP 1 fldAlternateSpeciesCode FROM dbo.tblReferenceMainSpecies WHERE fldMainSpeciesCode='", NotMeasuredSp$Species[s], "'", sep = "")
          aphia_data <- sqlQuery(channel, aphia_sql)
          aph <- aphia_data[1, 1]

          hlline <- paste(hlline, aph, sep = ",") #F13 - Spcode

          hlline <- paste(hlline, 4, sep = ",") #F14 - SpecVal

          # 10/02/2020 added stipulation to change berried B to female F
          if(NotMeasuredSp$Sex[s] == 'U') {
            sx <- -9
          } else if(NotMeasuredSp$Sex[s] == 'B') {
            sx <- 'F'
          } else {
            sx <- NotMeasuredSp$Sex[s]
          }

          hlline <- paste(hlline, sx, sep = ",") #F15 - Sex

          hlline <- paste(hlline, NotMeasuredSp$Count[s], sep = ",") #F16 - TotalNo

          hlline <- paste(hlline, 11, sep = ",") #F17 - CatIdentifier

          hlline <- paste(hlline, -9, sep = ",") #F18- NoMeas

          hlline <- paste(hlline, "1.0000", sep = ",") #F19 - SubFactor

          hlline <- paste(hlline, -9, sep = ",") #F20 - SubWgt

          NMcatch_weight <- round((NotMeasuredSp$Weight[s] * 1000), 0)

          hlline <- paste(hlline, NMcatch_weight, sep = ",") #F21 - CatCatchWgt

          hlline <- paste(hlline, -9, sep = ",") #F22 - Length class code

          hlline <- paste(hlline, -9 ,sep = ",") #F23 - Length class

          hlline <- paste(hlline, -9, sep = ",") #F24 - Number measured at length
          
          # code added to populate new devstage field
          if(NotMeasuredSp$Sex[s] == 'B') {
            dev <- 'B'
          } else {
            dev <- -9
          }
          
          hlline <- paste(hlline, dev, sep = ",") #F25 - DevStage (Development or maturity stage defined by non-invasive obs)

          hlline <- paste(hlline, -9, sep = ",") #F26 LenMeasType (Type of length measurement)

          cat(hlline, file = file, sep = "\n", append = TRUE)
          rm(hlline)

        }

      }

    }

  }

}

populateCAcore <- function(cruiseInfo, myVessel, ca_info, cruiseCode, cruiseSeries, file) {

  ca_data <- sqlQuery(channel, caCore_qry(cruiseCode))

  for (b in 1:nrow(ca_data)) {
    if(ca_data$SpCode[b] %in% c("COD","HAD","WHI","SAI","NPO","MAC","SPR","HER") && is.na(ca_data$Age[b]) == FALSE) {

      ca_gear_qry <- paste("SELECT * FROM dbo.tblReferenceMainGearCodes WHERE fldGearCode=", ca_data$GearCode[b], sep = "")
      ca_gear_detail <- sqlQuery(channel, ca_gear_qry)

      caline <- paste("CA", sep = ",") #CAF1 - RecordType
      caline <- paste(caline, cruiseInfo[["SurveyQuarter"]], sep = ",") #CAF2 - Quarter
      caline <- paste(caline, "GB-SCT", sep = ",") #CAF3 - Country
      caline <- paste(caline, "748S", sep = ",") #CAF4 - Ship
      caline <- paste(caline, ca_gear_detail$fldDATRASCode[1], sep = ",") #CAF5 - Gear
      caline <- paste(caline, ca_gear_detail$fldDATRASSweepLength[1], sep = ",") #CAF6 - Sweep length
      caline <- paste(caline, ca_gear_detail$fldDATRASGearException[1], sep = ",") #CAF7 - GearExp
      caline <- paste(caline, ca_gear_detail$fldDATRASDoorType[1], sep = ",") #CAF8 - DoorType
      caline <- paste(caline, ca_data$Haul[b], sep = ",") #CAF9 - StNo
      caline <- paste(caline, ca_data$Haul[b], sep = ",") #CAF10 - Haul
      caline <- paste(caline, ca_info[ca_info[, 1] == ca_data$Haul[b], 2], sep = ",") #CAF11 - Year
      caline <- paste(caline, "W", sep = ",") #CAF12 - Species Code Type

      aphia_sql = paste("SELECT TOP 1  fldAlternateSpeciesCode FROM dbo.tblReferenceMainSpecies WHERE fldMainSpeciesCode='", ca_data$SpCode[b], "'", sep = "")
      aphia_data <- sqlQuery(channel, aphia_sql)
      ####  sp <- ca_data$SpCode[b]
      aph <- aphia_data[1, 1]

      caline <- paste(caline, aph, sep = ",") #CAF13 - Species Code

      caline <- paste(caline, cruiseInfo[["SurveyAreaType"]], sep = ",") #CAF14 - Area Code Type (updated)

      #if (cruiseInfo[["SurveyAreaType"]] == 0) {
      #  stat_sql <- paste("SELECT TOP 1 LEFT(fldICESRectangle,2) AS P1, RIGHT(fldICESRectangle,2) AS P2 FROM tblDataStationLogs WHERE fldCruiseStationNumber=", ca_data$Haul[b], " AND fldCruiseName='", cruiseCode, "'", sep = "")
      #  statsq <- sqlQuery(channel, stat_sql)
      #  AreaCode <- paste(statsq[1, 1], statsq[1, 2], sep = "")
      #} else {
      #  acode_sql <- paste("SELECT TOP 1 fldStratum FROM dbo.tblDataStationLogs WHERE fldCruiseName='", cruiseCode,"' AND fldCruiseStationNumber=", ca_data$Haul[b], sep = "")
      #  acode <- sqlQuery(channel, acode_sql)
      #  AreaCode <- acode[, 1]
      #}

      stat_sql <- paste("SELECT TOP 1 LEFT(fldICESRectangle,2) AS P1, RIGHT(fldICESRectangle,2) AS P2 FROM tblDataStationLogs WHERE fldCruiseStationNumber=", ca_data$Haul[b], " AND fldCruiseName='", cruiseCode, "'", sep = "")
      statsq <- sqlQuery(channel, stat_sql)
      AreaCode <- paste(statsq[1, 1], statsq[1, 2], sep = "")

      caline <- paste(caline, AreaCode , sep = ",") #CAF15 - AreaCode

      caline <- paste(caline, ".", sep = ",") #CAF16 - LengthCode Type
      caline <- paste(caline, ca_data$Lngth[b], sep = ",") #CAF17 - Length class

      if(ca_data$Sex[b] == 'U') {
        sx = -9
      } else {
        sx = ca_data$Sex[b]
      }

      caline <- paste(caline, sx, sep = ",") #CAF18 - Sex

      if(is.na(ca_data$Maturity[b])) {
        mat = -9
      } else {
        if(ca_data$Maturity[b] == 'U') {
          mat = -9
        } else {
          #Specific translation of maturity codes for Herring needed
          if(ca_data$SpCode[b] == 'HER') {
            if(ca_data$Maturity[b] == 1 || ca_data$Maturity[b] == 2) {
              mat = 61
            } else if(ca_data$Maturity[b] == 3 || ca_data$Maturity[b] == 4 || ca_data$Maturity[b] == 5) {
              mat = 62
            } else if(ca_data$Maturity[b] == 6) {
              mat = 63
            } else {
              mat = 64
            }
          } else if(ca_data$SpCode[b] == 'MAC') {
            if(ca_data$Maturity[b] == 1) {
              mat = 61
            } else if(ca_data$Maturity[b] == 2 || ca_data$Maturity[b] == 3) {
              mat = 62
            } else if (ca_data$Maturity[b] == 4 || ca_data$Maturity[b] == 5) {
              mat = 63
            } else {
              mat = 64
            }
          } else if(cruiseSeries == 'NSIBTSQ1' || cruiseSeries == 'WCIBTSQ1') {
            mat = paste(6, ca_data$Maturity[b], sep = "")
          } else {
            mat = -9
          }
        }
      }

      caline <- paste(caline, mat, sep = ",") #CAF19 - Maturity

      caline <- paste(caline, -9, sep = ",") #CAF20 - Plus group

      if(is.na(ca_data$Age[b])) {age <- -9 } else {age <- ca_data$Age[b]}

      caline <- paste(caline, age, sep = ",") #CAF21 - Age rings

      caline <- paste(caline, ca_data$CaNoAtLen[b], sep = ",") #CAF22 - CANoatlegth

      if(is.na(ca_data$Weight[b])) {wgt <- -9 } else {wgt <- format(round(ca_data$Weight[b], 1), nsmall = 1)}

      caline <- paste(caline, wgt, sep = ",") #CAF23 - Weight
      
      # if maturity information is present then report the maturity scale used
      if(mat != -9) {
        matscale <- 'M6'
      } else {
        matscale <- -9
      }
          
      caline <- paste(caline, matscale, sep = ",") #CAF24 - MaturityScale
          
      caline <- paste(caline, -9, sep = ",") #CAF25 - FishID
          
      caline <- paste(caline, -9, sep = ",") #CAF26 - GenSamp
          
      caline <- paste(caline, -9, sep = ",") #CAF27 - StomSamp
          
      # if age information is present then report the age source used
      if(age != -9) {
        agesrc <- 'otolith'
      } else {
        agesrc <- -9
      }
          
      caline <- paste(caline, agesrc, sep = ",") #CAF28 - AgeSource
      caline <- paste(caline, -9, sep = ",") #CAF29 - AgePrepMet
      caline <- paste(caline, -9, sep = ",") #CAF30 - OtGrading
          
      caline <- paste(caline, -9, sep = ",") #CAF31 - ParSamp

      cat(caline, file = file, sep = "\n", append = TRUE)
      rm(caline)

    }

  }

}




populateCAnoncore <- function(cruiseInfo, myVessel, ca_info, cruiseCode, cruiseSeries, file) {

  ca_data <- sqlQuery(channel, caNonCore_qry(cruiseCode))

  for (b in 1:nrow(ca_data)) {
    #if(is.na(ca_data$Age[b]) == FALSE) { # Previously included only rows where there was age data
    if(!ca_data$SpCode[b] %in% c("COD","HAD","WHI","SAI","NPO","MAC","SPR","HER") && is.na(ca_data$Lngth[b]) == FALSE && is.na(ca_data$Weight[b]) == FALSE) { # Added 08/05/2017: add in all rows where both length and weight data available

      ca_gear_qry <- paste("SELECT * FROM dbo.tblReferenceMainGearCodes WHERE fldGearCode=", ca_data$GearCode[b], sep = "")
      ca_gear_detail <- sqlQuery(channel, ca_gear_qry)

      caline <- paste("CA", sep = ",") #CAF1 - RecordType
      caline <- paste(caline, cruiseInfo[["SurveyQuarter"]], sep = ",") #CAF2 - Quarter
      caline <- paste(caline, "GB-SCT", sep = ",") #CAF3 - Country
      caline <- paste(caline, "748S", sep = ",") #CAF4 - Ship
      caline <- paste(caline, ca_gear_detail$fldDATRASCode[1], sep = ",") #CAF5 - Gear
      caline <- paste(caline, ca_gear_detail$fldDATRASSweepLength[1], sep = ",") #CAF6 - Sweep length
      caline <- paste(caline, ca_gear_detail$fldDATRASGearException[1], sep = ",") #CAF7 - GearExp
      caline <- paste(caline, ca_gear_detail$fldDATRASDoorType[1], sep = ",") #CAF8 - DorrType
      caline <- paste(caline, ca_data$Haul[b], sep = ",") #CAF9 - StNo
      caline <- paste(caline, ca_data$Haul[b], sep = ",") #CAF10 - Haul
      caline <- paste(caline, ca_info[ca_info[, 1] == ca_data$Haul[b], 2], sep = ",") #CAF11 - Year
      caline <- paste(caline, "W", sep = ",") #CAF12 - Species Code Type

      aphia_sql = paste("SELECT TOP 1  fldAlternateSpeciesCode FROM dbo.tblReferenceMainSpecies WHERE fldMainSpeciesCode='", ca_data$SpCode[b], "'", sep = "")
      aphia_data <- sqlQuery(channel, aphia_sql)
    ####  sp <- ca_data$SpCode[b]
      aph <- aphia_data[1, 1]

      caline <- paste(caline, aph, sep = ",") #CAF13 - Species Code

      caline <- paste(caline, cruiseInfo[["SurveyAreaType"]], sep = ",") #CAF14 - Area Code Type (updated)

      #if (cruiseInfo[["SurveyAreaType"]] == 0) {
      #  stat_sql <- paste("SELECT TOP 1 LEFT(fldICESRectangle,2) AS P1, RIGHT(fldICESRectangle,2) AS P2 FROM tblDataStationLogs WHERE fldCruiseStationNumber=", ca_data$Haul[b], " AND fldCruiseName='", cruiseCode, "'", sep = "")
      #  statsq <- sqlQuery(channel, stat_sql)
      #  AreaCode <- paste(statsq[1, 1], statsq[1, 2], sep = "")
      #} else {
      #  acode_sql <- paste("SELECT TOP 1 fldStratum FROM dbo.tblDataStationLogs WHERE fldCruiseName='", cruiseCode,"' AND fldCruiseStationNumber=", ca_data$Haul[b], sep = "")
      #  acode <- sqlQuery(channel, acode_sql)
      #  AreaCode <- acode[, 1]
      #}

      stat_sql <- paste("SELECT TOP 1 LEFT(fldICESRectangle,2) AS P1, RIGHT(fldICESRectangle,2) AS P2 FROM tblDataStationLogs WHERE fldCruiseStationNumber=", ca_data$Haul[b], " AND fldCruiseName='", cruiseCode, "'", sep = "")
      statsq <- sqlQuery(channel, stat_sql)
      AreaCode <- paste(statsq[1, 1], statsq[1, 2], sep = "")

      caline <- paste(caline, AreaCode , sep = ",") #CAF15 - AreaCode

      caline <- paste(caline, ".", sep = ",") #CAF16 - LengthCode Type
      caline <- paste(caline, ca_data$Lngth[b], sep = ",") #CAF17 - Length class

      if(ca_data$Sex[b] == 'U') {
        sx = -9
      } else {
        sx = ca_data$Sex[b]
      }

      caline <- paste(caline, sx, sep = ",") #CAF18 - Sex

      caline <- paste(caline, "-9", sep = ",") #CAF19 - Maturity  ### Do not include maturity for non-core species

      caline <- paste(caline, -9, sep = ",") #CAF20 - Plus group

      if(is.na(ca_data$Age[b])) {age <- -9 } else {age <- ca_data$Age[b]}

      caline <- paste(caline, age, sep = ",") #CAF21 - Age rings

      caline <- paste(caline, ca_data$CaNoAtLen[b], sep = ",") #CAF22 - CANoatlegth

      if(is.na(ca_data$Weight[b])) {wgt <- -9 } else {wgt <- format(round(ca_data$Weight[b], 1), nsmall = 1)}

      caline <- paste(caline, wgt, sep = ",") #CAF23 - Weight
      
      # if maturity information is present then report the maturity scale used
      #if(mat != -9) {
      #  matscale <- 'M6'
      #} else {
        matscale <- -9
      #}
          
      caline <- paste(caline, matscale, sep = ",") #CAF24 - MaturityScale

      caline <- paste(caline, -9, sep = ",") #CAF25 - FishID
          
      caline <- paste(caline, -9, sep = ",") #CAF26 - GenSamp
          
      caline <- paste(caline, -9, sep = ",") #CAF27 - StomSamp
          
      # if age information is present then report the age source used
      if(age != -9) {
        agesrc <- 'otolith'
      } else {
        agesrc <- -9
      }
          
      caline <- paste(caline, agesrc, sep = ",") #CAF28 - AgeSource
      caline <- paste(caline, -9, sep = ",") #CAF29 - AgePrepMet
      caline <- paste(caline, -9, sep = ",") #CAF30 - OtGrading
      
      caline <- paste(caline, -9, sep = ",") #CAF31 - ParSamp
      
      cat(caline, file = file, sep = "\n", append = TRUE)
      rm(caline)

    }

  }

}

# 02/11/2017 - Updated the HH file to always receive stratum (previously -9) and the CA file to always receive stat sq (previously stratum or stat sq if no stratum available)
# 03/11/2020 - Updated the country code and ship code in HH, HL and CA
