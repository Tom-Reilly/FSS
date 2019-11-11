mapHaulTracks <- function(cruiseCodeSeries, xlims, ylims, icesRect = FALSE, write = FALSE) { # , userID

  if(missing(cruiseCodeSeries)) {stop("Please enter the cruise or cruises you wish to extract data from")}
  if(is.vector(cruiseCodeSeries) && length(cruiseCodeSeries) > 2) {stop("Please only supply two elements to the vector")}
  #if(write == TRUE & missing(userID)) {stop("Please provide a userID if you wish to write the output file")}
  #if(missing(userID)) {userID <- NULL}

  # Set switch based on whether a vector or dataframe has been supplied for cruiseCodeSeries
  if(is.vector(cruiseCodeSeries)) {codeSwitch <- 0}
  if(is.data.frame(cruiseCodeSeries)) {codeSwitch <- 1}

  dat <- chronDat(cruiseCodeSeries)
  dat[, c("ShotLat", "ShotLon", "HaulLat", "HaulLon")] <- sapply(dat[, c("ShotLat", "ShotLon", "HaulLat", "HaulLon")], as.numeric)

  if(missing(xlims)) {xlims <- c(min(pmin(dat$ShotLon, dat$HaulLon)) - 0.1, max(pmax(dat$ShotLon, dat$HaulLon)) + 0.1)}
  if(missing(ylims)) {ylims <- c(min(pmin(dat$ShotLat, dat$HaulLat)) - 0.1, max(pmax(dat$ShotLat, dat$HaulLat)) + 0.1)}

  if(xlims[1] < -18 | xlims[2] > 14 | ylims[1] < 48 | ylims[2] > 64) {stop("A value outside of the mapping margins has been selected")}

  setUpMap <- function(dat, ...) {

    cruises <- unique(paste(dat$CruiseName, dat$SeriesName, sep=" "))
    codes <- unique(dat$CruiseName)
    colourSet <- c("chocolate1","aquamarine3","azure4","blue3","brown2","darkorchid3","deeppink","yellow","gray19","green2",
                   "lightgoldenrod","lightpink","orangered4","royalblue","olivedrab","lightsalmon4","slateblue3","plum2","red4","mocassin","lightcyan4")
    colours <- colourSet[1:length(codes)]

      # Set layout
      layout(matrix(c(1,2), 2, 1, byrow=TRUE), heights=c(1,7))

      # Set legend
      par(mar=c(0, 0, 0, 0))
      plot(1, type="n", xlab="", ylab="", axes=FALSE, bty="n")
      legend("center", cruises, lwd = 2, ncol = ceiling(length(cruises)/4), col = colours, pt.cex = 0.65, bty="n", title = expression(bold("Cruise Haul Tracks")))

    # Set labels
    lons <- seq(-18, 14, 2)
    lonsLabs <- NULL

    for (i in 1:length(lons)) {
      if(lons[i] == 0) {lonsLabs[i] <- "0"
        } else if((lons[i] %% 2) == 0 & lons[i] < 0) {lonsLabs[i] <- paste(sub(".", "", lons[i]), "\U00B0", "W", sep = "")
          } else if((lons[i] %% 2) == 0 & lons[i] > 0) {lonsLabs[i] <- paste(lons[i], "\U00B0", "E", sep = "")
            } else {
              lonsLabs[i] <- ""
            }
    }

    lats <- seq(48, 64, 2)
    latsLabs <- NULL

    for (i in 1:length(lats)) {
      latsLabs[i] <- paste(lats[i], "\U00B0", "N", sep = "")
    }

    # Set plot
    #coast <- read.csv("Input/europe_coast_2.txt", header=T,sep="\t")
    par(mar=c(4.5, 4.5, 1, 1))
    #par(oma = c(4.5, 4.5, 1, 1))
    plot(coast,type="l",xlim=xlims, ylim=ylims, asp=1.5, xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE)

    if(icesRect == TRUE) {
      segments(seq(-20,16,1), 46, seq(-20,16,1), 68, lwd = 0.5, col = "gray60")
      segments(-20, seq(46,68,0.5), 16, seq(46,68,0.5), lwd = 0.5, col = "gray60")
    }

    axis(1, at = lons, labels = lonsLabs)
    axis(1, at = lons + 1, tcl = -0.2, labels = FALSE)
    axis(2, at = lats, labels = latsLabs)
    axis(2, at = lats + 1, tcl = -0.2, labels = FALSE)
    box()
    mtext("Longitude", side = 1, line = 3)
    mtext("Latitude", side = 2, line = 3)
    polygon(coast,col="chartreuse4")

    # Add lines if single cruise
    if(codeSwitch == 0) {
      segments(x0=dat$ShotLon,y0=dat$ShotLat,x1=dat$HaulLon,y1=dat$HaulLat, lwd = 2)
    }

    # Add lines if multiple cruises
    if(codeSwitch == 1) {
      for(i in 1:length(codes)) {
        segments(dat$ShotLon[codes[i] == dat$CruiseName],dat$ShotLat[codes[i] == dat$CruiseName],dat$HaulLon[codes[i] == dat$CruiseName],
                 dat$HaulLat[codes[i] == dat$CruiseName], col = colours[i], lwd = 2)
      }
    }
  }

  if(write==TRUE) {
    tiff(filename=paste("/map.tif", sep=""), width = 900, height = 900) # "Graphics/", userID,
    setUpMap(dat)
    dev.off()
  } else {
    setUpMap(dat)
  }

}
