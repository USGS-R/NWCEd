# # Remove NA values
# test2$prcp<-na.omit(test2$prcp)
#
# listoflists<-list(0)
# listoflists<-NULL
# startyear<-year(test2$prcp$date[1])
# endyear<-year(test2$prcp$date[nrow(test2$prcp)])+1
# # Cols<- paste(beginyear:endyear)
# # listoflists<-read.table(textConnection(""),col.names = Cols,colClasses = "character")
# # listoflists<-c(startyear:endyear)
# # wateryear<-c(startyear:endyear)
# watyr<-startyear
#
# # df<-data.frame(matrix(ncol=((endyear + 1)- startyear), nrow = 366))
# # colnames(df)<-paste0(c(startyear:endyear))
# #
# # foreach (i = iter(test2$prcp,by="row")) %do%{
# #   monthWeHave<-month(i$date)
# #   yearWeHave<-year(i$date)
# #   if(monthWeHave<09 & yearWeHave=watyr){
# #     listoflists$watyr[1]<-
# #     watyr=watyr+1
# #     #PUT DATA IN CURRENT WATER YEAR LIST
# #     wateryear<-year(i$date)
# #     listoflists$wateryear<-i$data
# #     print(listoflists$wateryear)
# #     print('wateryear')
#     print(yearWeHave)
#   }
#   else if (monthWeHave>=9){
#     #PUT DATA IN NEXT WATER YEAR LIST
#     yearWeHave<-yearWeHave+1
#     print(monthWeHave)
#     print('wateryear')
#     print(yearWeHave)
#   }
# }

#' Title
#'
#' @param inputData
#'
#' @return
#' @export
#'
#' @examples
Lp3<-function(inputData){

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
Ck <- n*SumCubeDiff/((n-1)*(n-2)*StandardDeviation)
Ck <- round(Ck,digits = 1)

}









