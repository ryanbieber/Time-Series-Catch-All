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
#' @examples DfRMSE <- best_forecast_by_RMSE(DF, data, f, DFError, trainStart)

best_forecast_by_RMSE <- function(DF, data,  f, DFError, trainStart){
  DfRMSE=list()
  ts_data <- xts(data, order.by = data$Date)
  ts_data<- t(ts_data)
  ts_data<- tslist(ts_data)
  for (i in 2:length(data)){
    #RMSE
    x <- colnames(data)
    namesErrorRMSE <- names(as.data.frame(DFError[[i]]))
    namesErrorRMSE <- namesErrorRMSE[match(min(DFError[[i]][2,]), DFError[[i]][2,])]
    orginal <-ts(ts_data[[i]], frequency = f, start = trainStart)
    DfRMSE[[i]] <- cbind(orginal, DF[[i]][,namesErrorRMSE])
    colnames(DfRMSE[[i]]) <- c(x[i], namesErrorRMSE)
  }
  return(DfRMSE)
}
