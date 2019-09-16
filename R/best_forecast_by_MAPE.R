#' Finding the best forecast by MAPE and outputting them into a list of data frames
#'
#' @param DF list of data frames from time_series_catch function
#' @param data original data with Date in first column
#' @param f frequency of data i.e. month/day/year 12/365/1, interger
#' @param DFError list of data frames that contain errors from error_calculations
#' @param trainStart numeric vector, i.e. c(2016, 1) for Jan 2016
#'
#' @return list of data frames with best forecast by that error method
#' @export
#'
#' @examples DfMAPE <- best_forecast_by_MAPE(DF, data, f, DFError, trainStart)
#'
best_forecast_by_MAPE <- function(DF, data,  f, DFError, trainStart){
  DfMAPE=list()
  ts_data <- xts(data, order.by = data$Date)
  ts_data<- t(ts_data)
  ts_data<- tslist(ts_data)
  for (i in 2:length(data)){
    #MAPE
    x <- colnames(data)
    namesErrorMAPE <- names(as.data.frame(DFError[[i]]))
    namesErrorMAPE <- namesErrorMAPE[match(min(DFError[[i]][1,]), DFError[[i]][1,])]
    orginal <-ts(ts_data[[i]], frequency = f, start = trainStart)
    DfMAPE[[i]] <- cbind(orginal, DF[[i]][,namesErrorMAPE])
    colnames(DfMAPE[[i]]) <- c(x[i], namesErrorMAPE)
  }
  return(DfMAPE)
}
