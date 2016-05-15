#' Get data from an NWC source.
#'
#' This function builds a request and returns the the data in question.
#'
#' @param huc The watershed of interest.
#' @param local TRUE/FALSE to request local watershed or total upstream watershed data.
#' @return The data.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @importFrom dataRetrieval readNWISdv
#' @export
#' @examples
#' NWCdata<-getNWCData(huc="031601030306")
#'
getNWCData<-function(huc, local=TRUE) {
  urls<-list(huc12=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc"),
             huc12agg=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta_agg.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet_agg.nc",
                        MEAN_streamflow="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc"),
             huc08=list(et="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC08_data/HUC08_eta.nc",
                        prcp="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC08_data/HUC08_daymet.nc"))
  nwisSite<-FALSE
  if(nchar(huc)==12 && local) {
    urlList<-urls['huc12'][[1]]
  } else if(nchar(huc)==12 && !local) {
    urlList<-urls['huc12agg'][[1]]
    nwisSite<-getNWISSite(huc)
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
    ts<-getSWECSVBlock(url)
    if (is.data.frame(ts)) {dataOut[name]<-list(ts)}
    }
  }
  if(is.character(nwisSite)) {
    dataOut['streamflow']<-list(readNWISdv(nwisSite,'00060'))
    names(dataOut$streamflow)[4]<-'data_00060_00003'
    names(dataOut$streamflow)[5]<-'cd_00060_00003'
  }
  dataOut$prcp$data[which(dataOut$prcp$data < 0)] <- NA
  try(names(dataOut)[which(names(dataOut) %in% 'MEAN_streamflow')]<-'streamflow',silent = TRUE)
  return(dataOut)
}
