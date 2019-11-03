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
#' @examples csv_forecast_values(data, DFMAPE, h, MAPE, DFError)
#'
csv_forecast_values <- function(data, errorlist, h, Models, DFError, x=1){
  e_val <- extract_best_error(data, DFError, x)

  for (i in 2:length(data)){
    errorlist[[i]] <- as.xts(errorlist[[i]])
  }

  out <- NULL
  for (i in 2:length(errorlist)) {
    out <- merge.xts(out, errorlist[[i]], join = "outer")
  }
  error_val <- rep(NA, NCOL(out))
  error_val[seq(2, NCOL(out),2)] <- e_val
  error_val <- t(data.frame(error_val))

  write.csv(out, file = "forecast_values.csv",row.names=FALSE)
  write.table(error_val, file = "forecast_values.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",", quote = FALSE)
}



