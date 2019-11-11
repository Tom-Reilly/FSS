\name{mapHaulTracks}
\alias{mapHaulTracks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map Cruise Haul Tracks
}
\description{
View or write a map displaying haul tracks for one or many cruises.
}
\usage{
mapHaulTracks(cruiseCodeSeries, xlims, ylims, icesRect = FALSE, write = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cruiseCodeSeries}{
a two element vector or a two column data frame containing the cruise code and cruise series to be displayed.
}
  \item{xlims}{
x axis plot limits as a two element vector identifying min and max extents.
}
  \item{ylims}{
y  axis plot limits as a two element vector identifying min and max extents.
}
  \item{icesRect}{
logical, if TRUE ICES rectangles will be displayed. Defaults to FALSE.
}
  \item{write}{
logical, if TRUE an image file will be written to your directory. Defaults to FALSE.
}
}
\seealso{
\code{\link{cruiseSelector}}
}
\examples{
# Display haul tracks for all 2016 cruises and display ICES rectangles
cruises <- cruiseSelector(years="2016")
mapHaulTracks(cruises, icesRect = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
