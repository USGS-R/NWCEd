#' Function to convert daily water budget data to an annualzed time series.
#'
#' Basic function that takes a date/data data frome and returns an annualized time series.
#'
#' @param inputData A daily data.frame containing a Date, format
#' "yyyy-mm-dd" 'date' and numerical 'data' collumn.
#' @return An annual time series data frame with a year and data collumn.
#' @export
#' @examples
#' url <- "http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc?request=GetObservation&service=SOS&version=1.0.0&observedProperty=prcp&offering=031401020800"
#' data<-getSWECSVBlock(url)
#' annual<-annualize(data)
#'
annualize<-function(inputData) {
  splitDate<-strsplit(as.character(inputData$date), "-")
  years<-as.character(lapply(splitDate,first<-function(x) x[1]))
  annual<-aggregate(inputData$data, list(year=years), mean)
  names(annual)<-c("year","data")
  return(annual)
}
