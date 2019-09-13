##
#' outputs a csv in wd called forecast values with the model name on the last row and all the forecasts above it (5)
#'
#' @param data original data containing Date in first column
#' @param errorlist list of best forecasts from best_forecast_by_MAPE/MSE/RMSE
#' @param h integer, number of forecast steps
#' @param Models a column vector of the bestmodels comes from name_of_best_model
#' @param DFError a list of data frames contianing all the errors comes from error_calculationsS
#' @param x indicator for MAPE=1, RMSE=2, MSE=3
#'
#' @return a matrix of forecasted values with the best model type used in the last row. Only able to use up to an h=3 value.
#' @export
#'
#' @examples fcast_values <- csv_forecast_values(data, DfMAPE, h, unique) takes the original data frame, the best_forecast_by_error_method, and the colname
#' is a vector of names for each column
csv_forecast_values <- function(data, errorlist, h, Models, DFError, x=1){
  colname <- as.factor(colnames(data)[2:length(data)])
  fcast <- na.omit(unlist(errorlist))
  forecast_values=list()
  e_val <- extract_best_error(data, DFError, x)
  if (x==1){
    error <- "MAPE"
  } else if (x==2){
    error <- "RMSE"
  } else (error <- "MSE")
  for (i in 1:h){
    fcast1 <- fcast[c(seq(NROW(data)+i, length(fcast), NROW(data)+h))]
    forecast_values[[i]] <- fcast1
  }
  if (h==1){
    fcast2 <- unlist(forecast_values)[c(1:length(data)-1)]
    x <- rbind(fcast2, t(Models), e_val)
    rownames(x) <- c("Forecast 1", "Model", error)
    colnames(x) <- colname
  } else if ( h==2 ){
    fcast2 <- unlist(forecast_values)[c(1:length(data)-1)]
    fcast3 <- unlist(forecast_values)[c(length(data):(length(data)*2-2))]
    x <- rbind(fcast2, fcast3, t(Models), e_val)
    rownames(x) <- c("Forecast 1", "Forecast 2", "Model", error)
    colnames(x) <- colname
  } else if ( h==3 ){
    fcast2 <- unlist(forecast_values)[c(1:length(data)-1)]
    fcast3 <- unlist(forecast_values)[c(length(data):(length(data)*2-2))]
    fcast4 <- unlist(forecast_values)[c((((length(data)-1)*2)+1):(length(data)*3-3))]
    x <- rbind(fcast2, fcast3, fcast4, t(Models), e_val)
    rownames(x) <- c("Forecast 1", "Forecast 2", "Forecast 3", "Model", error)
    colnames(x) <- colname
  }



  write.csv(x, file = "forecast_values.csv",row.names=TRUE)
  return(x)
}


