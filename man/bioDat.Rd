\name{bioDat}
\alias{bioDat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Biological Data
}
\description{
Extract all biological data (length, whole weight, gutted weight, sex, maturity, age) for the requested cruise(s) or extract biological data for a particular species or set of species. The output may be returned either as a data frame or as a list with each element representing a different cruise.
}
\usage{
bioDat(cruiseCodeSeries, species, as.List = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cruiseCodeSeries}{
a two element vector or a two column data frame containing the cruise code and cruise series to be queried.
}
  \item{species}{
character vector representing the species to be queried, using the FAO species 3-alpha code format. Defaults to include all species.
}
  \item{as.List}{
logical, specifying whether the output should be returned as either a data frame (FALSE) or a list (TRUE). Defaults to FALSE.
}
}
\details{
The \code{cruiseCodeSeries} can either be entered as a vector containing the cruise code and series for one particular cruise or as a data frame containing a number of cruises. The simplest method to ensure correct application of the \code{cruiseCodeSeries} parameter is to use the \code{\link{cruiseSelector}} function. The \code{cruiseCodeSeries} parameter is mandatory, however if there is a desire to output all biological data stored in FSS this can be achieved by running the \code{\link{cruiseSelector}} function with no filters. See Examples.
}
\seealso{
\code{\link{cruiseSelector}}
}
\examples{
# Extract biological data for all species for all cruises in 2016
cruises <- cruiseSelector(years = "2016")
bioDat(cruises)

# Extract monkfish biological data for all SIAMISS cruises and return as a list
cruises <- cruiseSelector(series = "SIAMISS")
bioDat(cruises, species = "ANF", as.List = TRUE)

# Extract all biological data from all cruises stored in FSS
cruises <- cruiseSelector()
bioDat(cruises)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
