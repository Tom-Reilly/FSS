\name{chronDat}
\alias{chronDat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Chronological Data
}
\description{
Extract cruise chronological data detailing haul gear, time, position, depth and validity. The output may be returned either as a data frame or as a list with each element representing a different cruise.
}
\usage{
chronDat(cruiseCodeSeries, as.List = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cruiseCodeSeries}{
a two element vector or a two column data frame containing the cruise code and cruise series to be queried.
}
  \item{as.List}{
logical, specifying whether the output should be returned as either a data frame (FALSE) or a list (TRUE). Defaults to FALSE.
}
}
\details{
The \code{cruiseCodeSeries} can either be entered as a vector containing the cruise code and series for one particular cruise or as a data frame containing a number of cruises. The simplest method to ensure correct application of the \code{cruiseCodeSeries} parameter is to use the \code{\link{cruiseSelector}} function. The \code{cruiseCodeSeries} parameter is mandatory, however if there is a desire to output all chronological data stored in FSS this can be achieved by running the \code{\link{cruiseSelector}} function with no filters. See Examples.
}
\seealso{
\code{\link{cruiseSelector}}
}
\examples{
# Extract all chronological data from West Coast IBTS Quarter 1 surveys
cruises <- cruiseSelector(quarters = 1, series = "WCIBTS")
chronDat(cruises)

# Extract chronological data from all cruises in FSS
cruises <- cruiseSelector()
chronDat(cruises)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
