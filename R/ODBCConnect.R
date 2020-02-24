ODBCConnect <- function(name, user, password) {  

  cat("Connecting to the FSS Database")

  cat("\n----------------------------------------------------------------------------\n")

  # Set up the connection to the FSS database
  channel <- odbcConnect(name, uid = user, pwd = password,  readOnly = TRUE)

  if(is(channel,"RODBC")) {cat("Connection with FSS Database established")
  } else {
    stop("A connection to the FSS Database could not be established")
  }

  cat("\n----------------------------------------------------------------------------\n")
  
  assign("channel", channel, envir = .GlobalEnv)
  
}
