#' Function to convert daily water budget data to an annualzed time series.
#'
#' Basic function that takes a date/data data frome and returns an annualized time series.
#'
#' @param inputData A daily data.frame containing a Date, format
#' "yyyy-mm-dd" 'date' and numerical 'data' collumn.
#' @param method A function to use in grouping. Defaults to mean, can be sum or other statistic.
#' @return An annual time series data frame with a year and data collumn.
#' @importFrom stats aggregate
#' @export
#' @examples
#' data<-getSWECSVBlock(system.file('extdata','SWECSVBlock_daymet_example.xml',package='NWCEd'))
#' annual<-annualize(data)
#'
annualize<-function(inputData, method = mean) {
  splitDate<-strsplit(as.character(inputData$date), "-")
  years<-as.character(lapply(splitDate,first<-function(x) x[1]))
  annual<-aggregate(inputData$data, list(year=years), method, na.rm=TRUE)
  names(annual)<-c("year","data")
  return(annual)
}
