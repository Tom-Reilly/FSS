\name{writeDATRAS}
\alias{writeDATRAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write Output for DATRAS
}
\description{
Extract the relevant data and output the file in the required format for submission to DATRAS.
}
\usage{
writeDATRAS(cruiseCodeSeries)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cruiseCodeSeries}{
a two element vector or a two column data frame containing the cruise code and cruise series to be queried.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A text file will be written to your directory containing the relevant HH, HL and CA lines.
}
\seealso{
\code{\link{cruiseSelector}}
}
\examples{
# Write the DATRAS file for the West Coast IBTS survey in quarter 1 2016
cruise <- cruiseSelector(years = 2016, quarters = 1, series = "WCIBTS")
writeDATRAS(cruise)

# Write the DATRAS file for the West Coast IBTS survey in quarter 1 2016 - Alternate method
writeDATRAS(c("0316S","WCIBTSQ1"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
