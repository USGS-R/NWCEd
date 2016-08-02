#' SkewCoeff function
#'
#' @param inputData
#' @param datatype
#'
#' @return printout of skew coefficients
#' @export
#' @importFrom foreach
#' @examples

Lp3<-function(inputData,datatype) {
print(datatype)
if (datatype == "prcp"){

  happyday$prcp<-na.omit(happyday$prcp)

dates<-c(happyday$prcp$date)

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
df = data.frame(dates,wtr_yr=wtr_yr(dates, 2),happyday$prcp$data)

split(df, df$wtr_yr)
df$dates<-NULL

test5<-split(df,f = df$wtr_yr)

# Finding max values for each water year

iterations = 150
variables = 1

MAX<- matrix(ncol=variables, nrow = iterations)
  foreach (i=iter(test5,by="row")) %do%{
    a<-max(i$happyday.prcp.data)
    MAX<-c(MAX,a)
  }
MAX<-na.omit(MAX)
print(MAX)
MAX<-data.frame(MAX)
print(MAX)
MAX$MAX<-MAX$MAX[order(-MAX$MAX)]
print(MAX$MAX)

n = nrow(MAX)
print(n)

RankMax = c(1:n)
print(RankMax)
# Finding Log(max)


LoggedMax<-matrix(ncol = variables, nrow = iterations)
  foreach (i=iter(MAX,by="row")) %do%{
    a<-log(i[1])
    LoggedMax<-c(LoggedMax,a)}
  print(LoggedMax)
  LoggedMax<-na.omit(LoggedMax)
  print(LoggedMax)
  LoggedMax<-data.frame(LoggedMax)
  print(LoggedMax)

# Finding the averages of the Max and log values
AverageMax<-mean(MAX$MAX)
print(AverageMax)
AverageLog<-mean(LoggedMax$LoggedMax)
print(AverageLog)
# {(loggedMax-mean(loggedMax))^2}


SquareDiff<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(LoggedMax,by="row")) %do%{
  a<-((i[1]-AverageLog)^2)
  SquareDiff<-c(SquareDiff,a)
}
SquareDiff<-na.omit(SquareDiff)
SquareDiff<-data.frame(SquareDiff)
print(SquareDiff)


# {(loggedMax-mean(loggedMax))^3}

CubeDiff<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(LoggedMax,by="row")) %do%{
  a<-((i[1]-AverageLog)^3)
  CubeDiff<-c(CubeDiff,a)
}
CubeDiff<-na.omit(CubeDiff)
CubeDiff<-data.frame(CubeDiff)
print(CubeDiff)
# Return Period {(n+1)/m}

ReturnPeriod<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(RankMax,by="row")) %do%{
  a<-((n+1)/i[1])
  ReturnPeriod<-c(ReturnPeriod,a)
}
ReturnPeriod<-na.omit(ReturnPeriod)
ReturnPeriod<-data.frame(ReturnPeriod)
print(ReturnPeriod)
# 1/Return Period

inverseReturnPeriod<-matrix(ncol = variables, nrow = iterations)
foreach (i=iter(ReturnPeriod,by="row")) %do%{
  a<-(1/i[1])
  inverseReturnPeriod<-c(inverseReturnPeriod,a)
}
inverseReturnPeriod<-na.omit(inverseReturnPeriod)
inverseReturnPeriod<-data.frame(inverseReturnPeriod)
print(inverseReturnPeriod)

SumSquareDiff<-sum(SquareDiff$SquareDiff)
print(SumSquareDiff)

SumCubeDiff<-sum(CubeDiff$CubeDiff)
print(SumCubeDiff)

Variance <- (SumSquareDiff/(n-1))
print(Variance)

StandardDeviation <- sqrt(Variance)
print(StandardDeviation)

Cs <- n*SumCubeDiff/((n-1)*(n-2)*StandardDeviation^3)
Cs<-round(Cs,digits=4)
print(Cs)
Cs2<-rep(Cs,61)

frequencyfactors<-read.csv('./Frequency_Factors_Log_Pearson_Type_III.csv')
frequencyfactors<-merge(frequencyfactors,Cs2)
frequencyfactors<-frequencyfactors[-c(62:3721),]
frequencyfactors<-frequencyfactors[frequencyfactors$Cs1 == frequencyfactors$y, ]
frequencyfactors$Cs1<-NULL
frequencyfactors$y<-NULL
frequencyfactors<-t(frequencyfactors)
print(frequencyfactors)

ReturnPeriod<-c(1.01,2,5,10,25,50,100,200)
ReturnPeriod<-data.frame(ReturnPeriod)
print(ReturnPeriod)

enddataframe<-matrix(ncol=variables,nrow=iterations)
  foreach (i = iter(frequencyfactors, by = "row")) %do%{
    a<-10^(AverageLog+(i[1]*StandardDeviation))
    enddataframe<-c(enddataframe,a)
  }

enddataframe<-enddataframe[!is.na(enddataframe)]
print(enddataframe)

x<-ReturnPeriod
y<-enddataframe

df<-cbind(x,y)

plotthis<-ggplot(df, aes(x=ReturnPeriod, y=y)) + geom_point() +
  xlab("Return Period (years)") + ylab("Precipitation (mm)") + ggtitle("Precipitation Frequency") +
  theme(panel.background = element_rect(fill = "grey75")) + geom_line(data = df, color="blue")
}

  else if ((datatype == "et")) {
  inputData$et<-na.omit(inputData$et)

  dates<-c(inputData$et$date)

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
  df = data.frame(dates,wtr_yr=wtr_yr(dates, 2),inputData$et$data)

  split(df, df$wtr_yr)
  df$dates<-NULL

  test5<-split(df,f = df$wtr_yr)

  # Finding max values for each water year

  iterations = 150
  variables = 1

  MAX<- matrix(ncol=variables, nrow = iterations)
  foreach (i=iter(test5,by="row")) %do%{
    a<-max(i$inputData.et.data)
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
  Cs <- n*SumCubeDiff/((n-1)*(n-2)*StandardDeviation)
  Cs<-round(Cs,digits=1)
  Cs2<-rep(Cs,61)

  frequencyfactors<-read.csv('./Frequency_Factors_Log_Pearson_Type_III.csv')
  frequencyfactors<-merge(frequencyfactors,Cs2)
  frequencyfactors<-frequencyfactors[-c(62:3721),]
  frequencyfactors<-frequencyfactors[frequencyfactors$Cs1 == frequencyfactors$y, ]
  frequencyfactors$Cs1<-NULL
  frequencyfactors$y<-NULL
  frequencyfactors<-t(frequencyfactors)
  print(frequencyfactors)

  ReturnPeriod<-c(1.01,2,5,10,25,50,100,200)
  ReturnPeriod<-data.frame(ReturnPeriod)
  print(ReturnPeriod)

  enddataframe<-matrix(ncol=variables,nrow=iterations)
  foreach (i = iter(frequencyfactors, by = "row")) %do%{
    a<-10^(AverageLog+i*StandardDeviation)
    enddataframe<-c(enddataframe,a)
  }

  enddataframe<-enddataframe[!is.na(enddataframe)]
  print(enddataframe)

  x<-ReturnPeriod
  y<-enddataframe

  df<-cbind(x,y)

  plotthis<-ggplot(df, aes(x=ReturnPeriod, y=y)) + geom_point() +
    xlab("Return Period (years)") + ylab("Runoff (cfs)") + ggtitle("Runoff Frequency") +
    theme(panel.background = element_rect(fill = "grey75")) + geom_line(data = df, color="yellow")
}
  return(plotthis)
  print(datatype)
}








