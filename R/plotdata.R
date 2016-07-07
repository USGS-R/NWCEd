#' Plot data from an NWC source.
#'
#' This function builds a request and returns the the data in question.
#'
#' @param Downloaded_Data The downloaded data to be plotted.
#' @return The plot.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @importFrom dataRetrieval readNWISdv
#' @import ggplot2
#' @export
#' @examples
#' data<-getNWCData(huc="031601030306")
plotdata<-function(Downloaded_Data) {
  data2 = subset(Downloaded_Data, data >= 0)
  plot(data2)
}
