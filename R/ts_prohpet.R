#' prohpet model
#'
#' @param data original data with Date in first column
#' @param h integer from 1-3
#' @param i iteration for loop
#'
#' @return forecasted values from prophet
#' @export
#'
#' @examples Uses the data, h and the i value from the loop in time_series_catch_all to get the forecast values.
ts_prophet <- function(data, h, i){
  dataprophet <- data[c(1,i)]
  colnames(dataprophet) <- c("ds", "y")
  dataprophettrain <- head(dataprophet, -h)
  dataprophettrain <- prophet(dataprophettrain, daily.seasonality=FALSE, weekly.seasonality=FALSE)
  future <- make_future_dataframe(dataprophettrain, periods = h, freq = 'month')
  forecast <- predict(dataprophettrain, future)
  fcastprophet <- tail(as.numeric(forecast$yhat),h)
  return(fcastprophet)
}
