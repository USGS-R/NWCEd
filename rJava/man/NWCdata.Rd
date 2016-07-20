\name{NWCdata}
\alias{NWCdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("NWCdata")}
\format{
  The format is:
List of 3
 $ et        :Classes 'dat' and 'data.frame':	180 obs. of  2 variables:
  ..$ date: Date[1:180], format: "2000-01-01" "2000-02-01" "2000-03-01" "2000-04-01" ...
  ..$ data: num [1:180] 23.8 32 30 50.4 67.2 ...
  ..- attr(*, "SRC")= chr "http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta_agg.nc?request=GetObservation&service=SOS&version=1.0.0&ob"| __truncated__
 $ prcp      :Classes 'dat' and 'data.frame':	12784 obs. of  2 variables:
  ..$ date: Date[1:12784], format: "1980-01-01" "1980-01-02" "1980-01-03" "1980-01-04" ...
  ..$ data: num [1:12784] 0 0 0 0 0 ...
  ..- attr(*, "SRC")= chr "http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet_agg.nc?request=GetObservation&service=SOS&version=1.0.0"| __truncated__
 $ streamflow:'data.frame':	8238 obs. of  5 variables:
  ..$ agency_cd       : chr [1:8238] "USGS" "USGS" "USGS" "USGS" ...
  ..$ site_no         : chr [1:8238] "09512165" "09512165" "09512165" "09512165" ...
  ..$ Date            : Date[1:8238], format: "1993-12-07" "1993-12-08" "1993-12-09" "1993-12-10" ...
  ..$ data_00060_00003: num [1:8238] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ cd_00060_00003  : chr [1:8238] "A" "A" "A" "A" ...
  ..- attr(*, "url")= chr "http://waterservices.usgs.gov/nwis/dv/?site=09512165&format=waterml,1.1&ParameterCd=00060&StatCd=00003&startDT=1851-01-01"
  ..- attr(*, "siteInfo")='data.frame':	1 obs. of  12 variables:
  .. ..$ station_nm          : chr "SALT RIVER AT PRIEST DRIVE NEAR PHOENIX, AZ."
  .. ..$ site_no             : chr "09512165"
  .. ..$ agency_cd           : chr "USGS"
  .. ..$ timeZoneOffset      : chr "-07:00"
  .. ..$ timeZoneAbbreviation: chr "MST"
  .. ..$ dec_lat_va          : num 33.4
  .. ..$ dec_lon_va          : num -112
  .. ..$ srs                 : chr "EPSG:4326"
  .. ..$ siteTypeCd          : Factor w/ 1 level "ST": 1
  .. .. ..- attr(*, "names")= chr "siteTypeCd"
  .. ..$ hucCd               : Factor w/ 1 level "15060106": 1
  .. .. ..- attr(*, "names")= chr "hucCd"
  .. ..$ stateCd             : Factor w/ 1 level "04": 1
  .. .. ..- attr(*, "names")= chr "stateCd"
  .. ..$ countyCd            : Factor w/ 1 level "04013": 1
  .. .. ..- attr(*, "names")= chr "countyCd"
  ..- attr(*, "variableInfo")='data.frame':	1 obs. of  6 variables:
  .. ..$ parameterCd   : chr "00060"
  .. ..$ parameter_nm  : chr "Streamflow, ft&#179;/s"
  .. ..$ parameter_desc: chr "Discharge, cubic feet per second"
  .. ..$ valueType     : chr "Derived Value"
  .. ..$ param_units   : chr "ft3/s"
  .. ..$ noDataValue   : logi NA
  ..- attr(*, "disclaimer")= Named chr "Provisional data are subject to revision. Go to http://waterdata.usgs.gov/nwis/help/?provisional for more information."
  .. ..- attr(*, "names")= chr "disclaimer"
  ..- attr(*, "statisticInfo")='data.frame':	1 obs. of  2 variables:
  .. ..$ statisticName: chr "Mean"
  .. ..$ statisticCd  : chr "00003"
  ..- attr(*, "queryTime")= POSIXct[1:1], format: "2016-06-29 23:23:30"
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
data(NWCdata)
## maybe str(NWCdata) ; plot(NWCdata) ...
}
\keyword{datasets}
