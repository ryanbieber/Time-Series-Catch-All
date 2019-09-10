##
#' outputs a csv in wd called forecast values with the model name on the last row and all the forecasts above it
#'
#' @param data original data containing Date in first column
#' @param errorlist list of best forecasts
#' @param h integer
#' @param colname character vector indicating column names
#'
#' @return a matrix of forecasted values with the best model type used in the last row. Only able to use up to an h=3 value.
#' @export
#'
#' @examples fcast_values <- csv_forecast_values(data, DfMAPE, h, unique) takes the original data frame, the best_forecast_by_error_method, and the colname
#' is a vector of names for each column
csv_forecast_values <- function(data, errorlist, h, colname){
  fcast <- na.omit(unlist(errorlist))
  forecast_values=list()
  for (i in 1:h){
    fcast1 <- fcast[c(seq(NROW(data)+i, length(fcast), NROW(data)+h))]
    forecast_values[[i]] <- fcast1
  }
  if (h==1){
    fcast2 <- unlist(forecast_values)[c(1:length(data)-1)]
    x <- rbind(fcast2, t(mapeModels))
    rownames(x) <- c("Forecast 1", "Model")
    colnames(x) <- colname
  } else if ( h==2 ){
    fcast2 <- unlist(forecast_values)[c(1:length(data)-1)]
    fcast3 <- unlist(forecast_values)[c(length(data):(length(data)*2-2))]
    x <- rbind(fcast2, fcast3, t(mapeModels))
    rownames(x) <- c("Forecast 1", "Forecast 2", "Model")
    colnames(x) <- colname
  } else if ( h==3 ){
    fcast2 <- unlist(forecast_values)[c(1:length(data)-1)]
    fcast3 <- unlist(forecast_values)[c(length(data):(length(data)*2-2))]
    fcast4 <- unlist(forecast_values)[c((((length(data)-1)*2)+1):(length(data)*3-3))]
    x <- rbind(fcast2, fcast3, fcast4, t(mapeModels))
    rownames(x) <- c("Forecast 1", "Forecast 2", "Forecast 3", "Model")
    colnames(x) <- colname
  }
  write.csv(x, file = "forecast_values.csv",row.names=TRUE)
  return(x)
}
