#' Get WFS polygon for a watershed.
#'
#' This function builds a request and returns geoJSON for the requested HUC.
#'
#' 8 digit HUCs are available for local watershed only.
#'
#' @param huc The watershed of interest.
#' @param local TRUE/FALSE to request local watershed or total upstream watershed data.
#' @return The data.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' NWCwatershed<-getNWCWatershed(huc="031601030306",local=TRUE)
#'
#' NWCwatershed_08 <- getNWCWatershed(huc="03160103",local=TRUE)
#'
getNWCWatershed<-function(huc,local) {

  baseURL<-"https://cida.usgs.gov/nwc/geoserver/ows"

  if (local) {
    if(nchar(huc)==12) {
      layer<-"WBD:huc12"
      hucRes<-"huc12"
    } else if(nchar(huc)==8) {
      layer <- "WBD:huc08"
      hucRes <- "huc8"
    }
  } else {
    if(nchar(huc)==12) {
      layer<-"WBD:huc12agg"
      hucRes<-"huc12"
    } else {
      stop("Only 12 digit hucs are available for local=F")
    }
  }

  filter<-URLencode(paste0("<Filter><PropertyIsEqualTo><PropertyName>",
                           hucRes, "</PropertyName><Literal>",
                           huc, "</Literal></PropertyIsEqualTo></Filter>"))

  dataURL<-paste0(baseURL,"?service=WFS&version=1.0.0&request=GetFeature&typeName=",
                  layer,"&filter=",filter,"&outputFormat=application/json&srsName=EPSG:4326")

  geojson<-fromJSON(txt=readLines(dataURL, warn = FALSE),
                    collapse = "\n",simplifyVector = FALSE)

  return(geojson)
}
