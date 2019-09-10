

#
#' Finding the best forecast by these error metrics and outputting them (3)
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
#' @examples DfMAPE <- best_forecast_by_MAPE(DF, data, f, DFError, trainStart, ts_data, vended_labor)
#' DF is a list of data frames, data is the original data frame that got iterated through, f is the frequency of the data, DFError is the list of data frames
#' with the error values in them, trainStart is a vector of the training start date e.g. c(2016, 1), ts_data is the xts matrix of data in time series format,
#' names is the original data frame with the column names to match.
best_forecast_by_MAPE <- function(DF, data,  f, DFError, trainStart, ts_data, names){
  DfMAPE=list()
  for (i in 2:length(data)){
    #MAPE
    x <- colnames(names)
    namesErrorMAPE <- names(as.data.frame(DFError[[i]]))
    namesErrorMAPE <- namesErrorMAPE[match(min(DFError[[i]][1,]), DFError[[i]][1,])]
    orginal <-ts(ts_data[[i]], frequency = f, start = trainStart)
    DfMAPE[[i]] <- cbind(orginal, DF[[i]][,namesErrorMAPE])
    colnames(DfMAPE[[i]]) <- c(x[i], namesErrorMAPE)
  }
  return(DfMAPE)
}
