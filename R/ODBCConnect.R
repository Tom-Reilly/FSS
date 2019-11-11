ODBCConnect <- function(name, location) {  

  cat("Connecting to the FSS Database")

  cat("\n----------------------------------------------------------------------------\n")
  
  if(location == "OSE") {password <- "mssfss"}
  if(location == "Scotia") {password <- "scotia"}

  # Set up the connection to the FSS database
  channel <- odbcConnect(name, uid = "rofss", pwd = password,  readOnly = TRUE)

  if(is(channel,"RODBC")) {cat("Connection with FSS Database established")
  } else {
    stop("A connection to the FSS Database could not be established")
  }

  cat("\n----------------------------------------------------------------------------\n")
  
  assign("channel", channel, envir = .GlobalEnv)
  
}