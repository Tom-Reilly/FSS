.onLoad <- function(libname = find.package("FSS"), pkgname = "FSS") {
  source("//SOSE0014F/Code/FSS_R_Package/config.R")
  ODBCConnect(name,user,password)
  rm(list=c("name","user","password"), envir = .GlobalEnv)
}
