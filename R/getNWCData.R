#' Get data from an NWC source.
#'
#' This function builds a request and returns the the data in question.
#'
#' @param huc The watershed of interest.
#' @return The data.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @export
#' @examples
#' data<-getNWCData(huc=031601030306")
#'
getNWCData<-function(huc) {
  urls<-list(huc12=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc",
                        MEAN_streamflow="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc"))
  if(nchar(huc)==12) {
    urlList<-urls['huc12'][[1]]
  } else { # Will implement huc08 here too.
    stop('must pass in a 12 - digit HUC identifier')
  }
  dataOut<-list()
  for (name in names(urlList)) {
    url<-paste0(urlList[name],'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',
                name,'&offering=',huc)
    # This is valid but not used now: ,'&eventTime=',startdate,'T00:00:00Z/', enddate,'T00:00:00Z'
    dataOut[name]<-list(getSWECSVBlock(url))
  }
  return(dataOut)
}
