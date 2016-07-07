\name{originalet}
\alias{originalet}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("originalet")}
\format{
  The format is:
List of 2
 $ et  :Classes 'dat' and 'data.frame':	180 obs. of  2 variables:
  ..$ date: Date[1:180], format: "2000-01-01" "2000-02-01" "2000-03-01" "2000-04-01" ...
  ..$ data: num [1:180] 13 20 30 57 123 135 152 147 99 37 ...
  ..- attr(*, "SRC")= chr "http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta.nc?request=GetObservation&service=SOS&version=1.0.0&observ"| __truncated__
 $ prcp:Classes 'dat' and 'data.frame':	12784 obs. of  2 variables:
  ..$ date: Date[1:12784], format: "1980-01-01" "1980-01-02" "1980-01-03" "1980-01-04" ...
  ..$ data: num [1:12784] 2 0 0 18 0 0 0 10 0 0 ...
  ..- attr(*, "SRC")= chr "http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc?request=GetObservation&service=SOS&version=1.0.0&obs"| __truncated__
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(originalet)
## maybe str(originalet) ; plot(originalet) ...
}
\keyword{datasets}
