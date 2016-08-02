#' PrecipLp3 function
#'
#' @param Cs
#' @param k1
#' @param k2
#' @param k3
#' @param k4
#' @param k5
#' @param k6
#' @param k7
#' @param k8
#'
#' @return printout of skew coefficient
#' @export
#' @importFrom ggplot2
#' @examples

PrecipLp3<-function(Cs,k1,k2,k3,k4,k5,k6,k7,k8){

test2$prcp<-na.omit(test2$prcp)

dates<-c(test2$prcp$date)

wtr_yr <- function(dates, start_month=9) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}
# Setting up the data
df = data.frame(dates,wtr_yr=wtr_yr(dates, 2),test2$prcp$data)

split(df, df$wtr_yr)
df$dates<-NULL

test5<-split(df,f = df$wtr_yr)

# Finding max values for each water year

iterations = 150
variables = 1

MAX<- matrix(ncol=variables, nrow = iterations)
  foreach (i=iter(test5,by="row")) %do%{
    a<-max(i$test2.prcp.data)
    MAX<-c(MAX,a)
  }
MAX<-na.omit(MAX)
MAX<-data.frame(MAX)
MAX$MAX<-MAX$MAX[order(-MAX$MAX)]

n = nrow(MAX)

RankMax = c(1:n)

# Finding Log(max)


LoggedMax<-matrix(ncol = variables, nrow = iterations)
  foreach (i=iter(MAX,by="row")) %do%{
    a<-log(i)
    LoggedMax<-c(LoggedMax,a)
  }
LoggedMax<-na.omit(LoggedMax)
LoggedMax<-data.frame(LoggedMax)

# Finding the averages of the Max and log values
AverageMax<-mean(MAX$MAX)
AverageLog<-mean(LoggedMax$LoggedMax)

# {(loggedMax-mean(loggedMax))^2}


SquareDiff<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(LoggedMax,by="row")) %do%{
  a<-((i-AverageLog)^2)
  SquareDiff<-c(SquareDiff,a)
}
SquareDiff<-na.omit(SquareDiff)
SquareDiff<-data.frame(SquareDiff)

# {(loggedMax-mean(loggedMax))^3}

CubeDiff<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(LoggedMax,by="row")) %do%{
  a<-((i-AverageLog)^3)
  CubeDiff<-c(CubeDiff,a)
}
CubeDiff<-na.omit(CubeDiff)
CubeDiff<-data.frame(CubeDiff)

# Return Period {(n+1)/m}

ReturnPeriod<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(RankMax,by="row")) %do%{
  a<-((n+1)/i)
  ReturnPeriod<-c(ReturnPeriod,a)
}
ReturnPeriod<-na.omit(ReturnPeriod)
ReturnPeriod<-data.frame(ReturnPeriod)

# 1/Return Period

inverseReturnPeriod<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(ReturnPeriod,by="row")) %do%{
  a<-(1/i)
  inverseReturnPeriod<-c(inverseReturnPeriod,a)
}
inverseReturnPeriod<-na.omit(inverseReturnPeriod)
inverseReturnPeriod<-data.frame(inverseReturnPeriod)

SumSquareDiff<-sum(SquareDiff$SquareDiff)
SumCubeDiff<-sum(CubeDiff)

Variance <- (SumSquareDiff/(n-1))
StandardDeviation <- sqrt(Variance)
Ck <- Cs

ReturnPeriod<-c(1.01,2,5,10,25,50,100,200)

kcoeff<-c(k1,k2,k3,k4,k5,k6,k7,k8)

endval1<-10^(AverageLog+k1*StandardDeviation)
endval2<-10^(AverageLog+k2*StandardDeviation)
endval3<-10^(AverageLog+k3*StandardDeviation)
endval4<-10^(AverageLog+k4*StandardDeviation)
endval5<-10^(AverageLog+k5*StandardDeviation)
endval6<-10^(AverageLog+k6*StandardDeviation)
endval7<-10^(AverageLog+k7*StandardDeviation)
endval8<-10^(AverageLog+k8*StandardDeviation)

endval<-c(endval1,endval2,endval3,endval4,endval5,endval6,endval7,endval8)

ReturnPeriodandEndval<-data.frame(ReturnPeriod,endval)

plotthis<-ggplot(ReturnPeriodandEndval, aes(x=ReturnPeriod, y=endval)) + geom_point() +
xlab("Return Period (years)") + ylab("Precipitation (mm)") + ggtitle("Precipitation Frequency") +
theme(panel.background = element_rect(fill = "grey75")) + geom_line(data = ReturnPeriodandEndval, color="blue")

return(plotthis)

}









