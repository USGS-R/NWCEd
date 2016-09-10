#' The Lp3 function applies the Log-Pearson Type III model to hydrologic datasets from the National Water Census Data Portal to generate frequency distribution curves.
#'
#' The purpose of the Lp3 function is to apply the Log-Pearson Type III model to streamflow and precipitation datasets from the National Water Census Data Portal to obtain their respective frequency distribution curves.
#' The desired datasets are to be previously downloaded from the Portal using the 'getNWCData' function and saved in a user-named variable. The user inputs two arguments into the Lp3 function: inputData (the user-named variable) and the desired dataset ("streamflow" or "prcp").
#' The function singles out the desired dataset and and converts units from metric to english. The dataset is organized into water years. Next, the Log Pearson Type III model is applied to the data. The function calls in Frequency Factors from a csv.
#' Freqency factors were taken from http://streamflow.engr.oregonstate.edu/analysis/floodfreq/skew.htm.
#'
#' @param inputData \code{data.frame} stored in user-named variable, returned by getNWCData
#' @param datatype \code{character} choose from "streamflow" and "prcp"
#'
#' @return The return variable "plotthis" is a plot of the Log-Pearson Type III model applied to the user-selected USGS hydrologic dataset.
#' @export
#' @importFrom foreach foreach
#' @importFrom iterators iter
#' @examples
#' \dontrun{
#' variable_name<-getNWCData(huc = "160202030505", local = FALSE)
#' Lp3(variable_name, "prcp")
#' }
Lp3<-function(inputData,datatype) {
  if (datatype == "prcp"){

    inputData$prcp<-na.omit(inputData$prcp)
    inputData$prcp$data<-inputData$prcp$data*0.0393701
    dates<-c(inputData$prcp$date)

    # Setting up the data
    df = data.frame(dates,wtr_yr=wtr_yr(dates, 2),inputData$prcp$data)

    split(df, df$wtr_yr)
    df$dates<-NULL

    test5<-split(df,f = df$wtr_yr)

    # Finding max values for each water year

    iterations = 150
    variables = 1

    MAX<- matrix(ncol=variables, nrow = iterations)
    foreach (i=iter(test5,by="row")) %do%{
      a<-max(i$inputData.prcp.data)
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
      a<-log10(i[1])
      LoggedMax<-c(LoggedMax,a)}
    LoggedMax<-na.omit(LoggedMax)
    LoggedMax<-data.frame(LoggedMax)

    # Finding the averages of the Max and log values
    AverageMax<-mean(MAX$MAX)
    AverageLog<-mean(LoggedMax$LoggedMax)

    # {(loggedMax-mean(loggedMax))^2}
    SquareDiff<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(LoggedMax,by="row")) %do%{
      a<-((i[1]-AverageLog)^2)
      SquareDiff<-c(SquareDiff,a)
    }
    SquareDiff<-na.omit(SquareDiff)
    SquareDiff<-data.frame(SquareDiff)

    # {(loggedMax-mean(loggedMax))^3}
    CubeDiff<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(LoggedMax,by="row")) %do%{
      a<-((i[1]-AverageLog)^3)
      CubeDiff<-c(CubeDiff,a)
    }
    CubeDiff<-na.omit(CubeDiff)
    CubeDiff<-data.frame(CubeDiff)

    # Return Period {(n+1)/m}
    ReturnPeriod<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(RankMax,by="row")) %do%{
      a<-((n+1)/i[1])
      ReturnPeriod<-c(ReturnPeriod,a)
    }
    ReturnPeriod<-na.omit(ReturnPeriod)
    ReturnPeriod<-data.frame(ReturnPeriod)

    # 1/Return Period
    inverseReturnPeriod<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(ReturnPeriod,by="row")) %do%{
      a<-(1/i[1])
      inverseReturnPeriod<-c(inverseReturnPeriod,a)
    }
    inverseReturnPeriod<-na.omit(inverseReturnPeriod)
    inverseReturnPeriod<-data.frame(inverseReturnPeriod)

    SumSquareDiff<-sum(SquareDiff$SquareDiff)

    SumCubeDiff<-sum(CubeDiff$CubeDiff)

    Variance <- (SumSquareDiff/(n-1))

    StandardDeviation <- sqrt(Variance)

    # adds column of Skew factors to log-Pearson Type III Distributions table (Haan,1977,Table 7.7)
    Cs <- n*SumCubeDiff/((n-1)*(n-2)*StandardDeviation^3)
    Cs<-round(Cs,digits=1)
    Cs2<-rep(Cs,61)

    # matches skew coefficients and extracts frequency factors from same row
    frequencyfactors<-merge(frequencyfactors,Cs2)
    frequencyfactors<-frequencyfactors[-c(62:3721),]
    frequencyfactors<-frequencyfactors[frequencyfactors$Cs1 == frequencyfactors$y, ]
    frequencyfactors$Cs1<-NULL
    frequencyfactors$y<-NULL
    frequencyfactors<-t(frequencyfactors)

    # Creates recurrence table
    ReturnPeriod<-c(1.01,2,5,10,25,50,100,200)
    ReturnPeriod<-data.frame(ReturnPeriod)

    # Performs general equation to obtain new log values
    enddataframe<-matrix(ncol=variables,nrow=iterations)
    foreach (i = iter(frequencyfactors, by = "row")) %do%{
      a<-10^(AverageLog+(i[1]*StandardDeviation))
      enddataframe<-c(enddataframe,a)
    }

    enddataframe<-enddataframe[!is.na(enddataframe)]

    # combines new dataframe with new log values and recurrence interval
    x<-ReturnPeriod
    y<-enddataframe

    df<-cbind(x,y)

    # Plots the graph showing new log values and recurrence interval
    plotthis<-ggplot(df, aes(x=ReturnPeriod, y=y)) + geom_point() +
      xlab("Return Period (years)") + ylab("Precipitation (inches)") + ggtitle("Precipitation Frequency") +
      theme(panel.background = element_rect(fill = "grey75")) + geom_line(data = df, color="blue")
  }

  else if ((datatype == "streamflow")) {
    inputData$streamflow$agency_cd<-NULL
    inputData$streamflow$site_no<-NULL
    inputData$streamflow$cd_00060_00003<-NULL
    names(inputData$streamflow)[names(inputData$streamflow)=="data_00060_00003"]<-"data"
    names(inputData$streamflow)[names(inputData$streamflow)=="Date"]<-"date"

    inputData$streamflow<-na.omit(inputData$streamflow)

    dates<-c(inputData$streamflow$date)

    # Setting up the data
    df = data.frame(dates,wtr_yr=wtr_yr(dates, 2),inputData$streamflow$data)

    split(df, df$wtr_yr)
    df$dates<-NULL

    test5<-split(df,f = df$wtr_yr)

    # Finding max values for each water year

    iterations = 150
    variables = 1

    MAX<- matrix(ncol=variables, nrow = iterations)
    foreach (i=iter(test5,by="row")) %do%{
      a<-max(i$inputData.streamflow.data)
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
      a<-log10(i[1])
      LoggedMax<-c(LoggedMax,a)}
    LoggedMax<-na.omit(LoggedMax)
    LoggedMax<-data.frame(LoggedMax)

    # Finding the averages of the Max and log values
    AverageMax<-mean(MAX$MAX)
    AverageLog<-mean(LoggedMax$LoggedMax)

    # {(loggedMax-mean(loggedMax))^2}
    SquareDiff<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(LoggedMax,by="row")) %do%{
      a<-((i[1]-AverageLog)^2)
      SquareDiff<-c(SquareDiff,a)
    }
    SquareDiff<-na.omit(SquareDiff)
    SquareDiff<-data.frame(SquareDiff)

    # {(loggedMax-mean(loggedMax))^3}
    CubeDiff<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(LoggedMax,by="row")) %do%{
      a<-((i[1]-AverageLog)^3)
      CubeDiff<-c(CubeDiff,a)
    }
    CubeDiff<-na.omit(CubeDiff)
    CubeDiff<-data.frame(CubeDiff)

    # Return Period {(n+1)/m}
    ReturnPeriod<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(RankMax,by="row")) %do%{
      a<-((n+1)/i[1])
      ReturnPeriod<-c(ReturnPeriod,a)
    }
    ReturnPeriod<-na.omit(ReturnPeriod)
    ReturnPeriod<-data.frame(ReturnPeriod)

    # 1/Return Period
    inverseReturnPeriod<-matrix(ncol = variables, nrow = iterations)
    foreach (i=iter(ReturnPeriod,by="row")) %do%{
      a<-(1/i[1])
      inverseReturnPeriod<-c(inverseReturnPeriod,a)
    }
    inverseReturnPeriod<-na.omit(inverseReturnPeriod)
    inverseReturnPeriod<-data.frame(inverseReturnPeriod)

    SumSquareDiff<-sum(SquareDiff$SquareDiff)

    SumCubeDiff<-sum(CubeDiff$CubeDiff)

    Variance <- (SumSquareDiff/(n-1))

    StandardDeviation <- sqrt(Variance)

    Cs <- n*SumCubeDiff/((n-1)*(n-2)*StandardDeviation^3)
    Cs<-round(Cs,digits=1)
    Cs2<-rep(Cs,61)

    frequencyfactors<-read.csv('./Frequency_Factors_Log_Pearson_Type_III.csv')
    frequencyfactors<-merge(frequencyfactors,Cs2)
    frequencyfactors<-frequencyfactors[-c(62:3721),]
    frequencyfactors<-frequencyfactors[frequencyfactors$Cs1 == frequencyfactors$y, ]
    frequencyfactors$Cs1<-NULL
    frequencyfactors$y<-NULL
    frequencyfactors<-t(frequencyfactors)

    ReturnPeriod<-c(1.01,2,5,10,25,50,100,200)
    ReturnPeriod<-data.frame(ReturnPeriod)

    enddataframe<-matrix(ncol=variables,nrow=iterations)
    foreach (i = iter(frequencyfactors, by = "row")) %do%{
      a<-10^(AverageLog+(i[1]*StandardDeviation))
      enddataframe<-c(enddataframe,a)
    }

    enddataframe<-enddataframe[!is.na(enddataframe)]

    x<-ReturnPeriod
    y<-enddataframe

    df<-cbind(x,y)

    plotthis<-ggplot(df, aes(x=ReturnPeriod, y=y)) + geom_point() +
      xlab("Return Period (years)") + ylab("Streamflow (CFS)") + ggtitle("Streamflow Frequency") +
      theme(panel.background = element_rect(fill = "grey75")) + geom_line(data = df, color="yellow")
    }
    else {stop("Didn't get a datatype of 'prcp' or 'streamflow', must choose one or the other for the Lp3 function.")}
  return(plotthis)
}

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

