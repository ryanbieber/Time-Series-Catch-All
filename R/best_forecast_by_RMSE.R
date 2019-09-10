#' Finding the best forecast by these error metrics and outputting them
#'
#' @param DF list of data from time_series_catch
#' @param data original data with Date in first column
#' @param f integer
#' @param DFError list of data frames that contain errors
#' @param trainStart numeric vector
#' @param ts_data xts matrix
#' @param names character vector
#'
#' @return list of data frames with best forecast by that error method
#' @export
#'
#' @examples DfRMSE <- best_forecast_by_RMSE(DF, data, f, DFError, trainStart, ts_data, vended_labor)
#' DF is a list of data frames, data is the original data frame that got iterated through, f is the frequency of the data, DFError is the list of data frames
#' with the error values in them, trainStart is a vector of the training start date e.g. c(2016, 1), ts_data is the xts matrix of data in time series format,
#' names is the original data frame with the column names to match.
best_forecast_by_RMSE <- function(DF, data,  f, DFError, trainStart, ts_data, names){
  for (i in 2:length(data)){
    DfRMSE=list()
    #RMSE
    x <- colnames(names)
    namesErrorRMSE <- names(as.data.frame(DFError[[i]]))
    namesErrorRMSE <- namesErrorRMSE[match(min(DFError[[i]][2,]), DFError[[i]][2,])]
    orginal <-ts(ts_data[[i]], frequency = f, start = trainStart)
    DfRMSE[[i]] <- cbind(orginal, DF[[i]][,namesErrorRMSE])
    colnames(DfRMSE[[i]]) <- c(x[i], namesErrorRMSE)
  }
  return(DfRMSE)
}
