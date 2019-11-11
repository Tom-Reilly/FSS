\name{cruiseSelector}
\alias{cruiseSelector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select cruises
}
\description{
Select those cruises from which you want to extract data. Select filters from year, quarter, crusie start month and cruise series. The output can be used in the other package functions.
}
\usage{
cruiseSelector(years, quarters, startMonth, series)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{years}{
character, detailing the year(s) you want to search for. Either YY or YYYY format.
}
  \item{quarters}{
integer, detailing the quarter(s) you want to search for.
}
  \item{startMonth}{
integer, detailing the cruise start months you want to search for.
}
  \item{series}{
character, detailing the cruise series you want to search for. Available options are "DEEPWATER", "GEARTRIALS", "HERAS", "MACAS", "NSIBTS", "ROCKALL", "SIAMISS", "SKATE", "WCIBTS".
}
}
\value{
A two column data frame will be returned containing the cruise code and cruise series for the selected cruise(s).
}
\seealso{
\code{\link{chronDat}}, \code{\link{lengthDat}}, \code{\link{bioDat}}
}
\examples{
# Select all cruises from 2016 in quarters 1 and 2
cruiseSelector(years = "2016", quarters = 1:2)

# Select West Coast IBTS cruises from 2014 to 2016
cruiseSelector(years = c(2014,2015,2016), series = "WCIBTS")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
