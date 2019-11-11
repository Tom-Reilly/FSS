.onLoad <- function(libname = find.package("FSS"), pkgname = "FSS") {
  ODBCConnect("FSS","OSE")
}
