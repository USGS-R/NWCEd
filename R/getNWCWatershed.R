#' Get WFS polygon for a watershed.
#'
#' This function builds a request and returns a spatial polygons data frame.
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
getNWCWatershed<-function(huc,local) {

  baseURL<-"https://cida.usgs.gov/nwc/geoserver/ows"

  if (local) {
    layer<-"WBD:huc12"
  } else {
    layer<-"WBD:huc12agg"
  }

  hucRes<-'huc12'

  filter<-URLencode(paste0("<Filter><PropertyIsEqualTo><PropertyName>",
                           hucRes, "</PropertyName><Literal>",
                           huc, "</Literal></PropertyIsEqualTo></Filter>"))

  dataURL<-paste0(baseURL,"?service=WFS&version=1.0.0&request=GetFeature&typeName=",
                  layer,"&filter=",filter,"&outputFormat=application/json&srsName=EPSG:4326")

  geojson<-fromJSON(txt=readLines(dataURL, warn = FALSE),
                    collapse = "\n",simplifyVector = FALSE)

  return(geojson)
}
