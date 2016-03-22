#' Get data from an NWC source.
#'
#' This function builds a request and returns the the data in question.
#'
#' @param dataSource One of the allowed data sources.
#' @param huc The watershed of interest.
#' @return The data.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @export
#' @importFrom NWCCompare SWE_CSV_IHA
#' @examples
#' newData<-getNWCData(dataSource="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc",huc="031401020800")
#'
getNWCData<-function(dataSource,huc) {
  observedProperty="prcp"
  url<-paste0(dataSource,'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',
             observedProperty,'&offering=',huc)
              # This is valid but not used now: ,'&eventTime=',startdate,'T00:00:00Z/', enddate,'T00:00:00Z'
  dataOut<-SWE_CSV_IHA(url)
}
