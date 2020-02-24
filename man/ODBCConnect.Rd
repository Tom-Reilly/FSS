\name{ODBCConnect}
\alias{ODBCConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Connection to FSS
}
\description{
Create an Online Database Connection (ODBC) to the Fisheries Survey System (FSS) Database. It is necessary to run this function prior to any further attempts to query the FSS database.
}
\usage{
ODBCConnect(name, user, password)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
character, the name of the database, in this case "FSS".
}
  \item{user}{
character, the user name defined when setting up the DSN, this will differ between location, OSE or Scotia.
}
  \item{password}{
    character, the passord defined when setting up the DSN, this will differ between location, OSE or Scotia.
    }
}
\examples{
# Connect to FSS from the OSE
ODBCConnect(name = "FSS", user = "FSSuser", password = "pwd")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
