#' Get data from an NWC source.
#'
#' This function builds a request and returns the the data in question.
#'
#' @param huc The watershed of interest.
#' @return The data.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @export
#' @examples
#' data<-getNWCData(huc="031601030306")
#'
getNWCData<-function(huc, local=TRUE) {
  urls<-list(huc12=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc"),
             huc12agg=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta_agg.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet_agg.nc",
                        MEAN_streamflow="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc"),
             huc08=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC08_data/HUC08_eta.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC08_data/HUC08_daymet.nc"))

  if(nchar(huc)==12 && local) {
    urlList<-urls['huc12'][[1]]
  } else if(nchar(huc)==12 && !local) {
    urlList<-urls['huc12agg'][[1]]
  } else if (nchar(huc)==8 && local) {
    urlList<-urls['huc08'][[1]]
  } else if (nchar(huc)==8 && !local) {
    stop('Total upstream HUC08 watersheds are not available yet.')
  } else { # Will implement huc08 here too.
    stop('must pass in an 8 or 12 digit HUC identifier')
  }
  dataOut<-list()
  for (name in names(urlList)) {
    if(grepl(pattern = 'nwc/thredds/sos', x = urlList[name])) {
    url<-paste0(urlList[name],'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',
                name,'&offering=',huc)
    # This is valid but not used now: ,'&eventTime=',startdate,'T00:00:00Z/', enddate,'T00:00:00Z'
    dataOut[name]<-list(getSWECSVBlock(url))
    }
  }
  return(dataOut)
}
