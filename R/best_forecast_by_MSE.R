
#' Finding the best forecast by these error metrics and outputting them
#'
#' @param DF list of data from time_series_catch
#' @param data original data with Date in first column
#' @param f frequency of data i.e. month/day/year 12/365/1, interger
#' @param DFError list of data frames that contain errors from error_calculations
#' @param trainStart numeric vector, i.e. c(2016, 1) for Jan 2016
#'
#' @return list of data frames with best forecast by that error method
#' @export
#'
#' @examples DfMSE <- best_forecast_by_MSE(DF, data, f, DFError, trainStart)
#'
best_forecast_by_MSE <- function(DF, data, f, DFError, trainStart){
  DfMSE=list()
  ts_data <- xts(data, order.by = data$Date)
  ts_data<- t(ts_data)
  ts_data<- tslist(ts_data)
  for (i in 2:length(data)){
    #MSE
    x <- colnames(data)
    namesErrorMSE <- names(as.data.frame(DFError[[i]]))
    namesErrorMSE <- namesErrorMSE[match(min(DFError[[i]][3,]), DFError[[i]][3,])]
    orginal <-ts(ts_data[[i]], frequency = f, start = trainStart)
    DfMSE[[i]] <- cbind(orginal, DF[[i]][,namesErrorMSE])
    colnames(DfMSE[[i]]) <- c(x[i], namesErrorMSE)
  }
  return(DfMSE)
}


