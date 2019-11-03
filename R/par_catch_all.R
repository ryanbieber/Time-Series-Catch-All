## data goes into a list of dataframes

#' The big dawg that will forecast with the best of them
#'
#' @param x list of time series you want to forecast
#' @param freq character string indicating "month" for monthly etc.
#' @param steps integer that explains how far out you want forecast(needed for DLM)
#' @param dlmPoly integer for the order of polynomial you want in dlm, default = 2
#' @param dlmSeas integer for seasonal effect in dlm, default is 12 for monthly
#' @param num.cores integer for the number of cores you want to use
#' @param a.a.args list of arguments you want to change
#' @param ets.args list of arguments you want to change
#' @param tbats.args list of arguments you want to change
#' @param prophet.args list of arguments you want to change
#' @param n.n.args list of arguments you want to change
#'
#' @return list of models for eacg ts
#' @export
#'
#' @examples test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#' test_forecast <- extract_model_fit_forecast(test_model, num.cores = 12)
#'
#'
#'
par_time_series_catch <- function(x, freq = "month", steps = 3, dlmPoly = 2, dlmSeas = 12, num.cores = 2, a.a.args = list(NULL),
                                  ets.args = list(NULL), tbats.args = list(NULL), prophet.args = list(NULL), n.n.args = list(NULL)){
  auto_args <- list(max.p = 5, max.q = 5, max.P = 2,
                    max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                    start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                    seasonal = TRUE, nmodels = 94)

  ets_args <- list(model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL,
                   gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                   biasadj = FALSE)
  tbats_args <- list(use.box.cox = NULL, use.trend = NULL,
                     use.damped.trend = NULL, seasonal.periods = NULL,
                     use.arma.errors = TRUE)
  prophet_args <- list(growth = "linear", changepoints = NULL,
                       n.changepoints = 25, changepoint.range = 0.8,
                       yearly.seasonality = "auto", weekly.seasonality = "auto",
                       daily.seasonality = "auto", holidays = NULL,
                       seasonality.mode = "additive")
  n_n_args <- list(P = 1, repeats = 20, xreg = NULL, lambda = NULL,
                   model = NULL, subset = NULL, scale.inputs = TRUE)
  auto_args <- modifyList(auto_args, a.a.args)
  ets_args <- modifyList(ets_args, ets.args)
  tbats_args <- modifyList(tbats_args, tbats.args)
  prophet_args <- modifyList(prophet_args, prophet.args)
  n_n_args <- modifyList(n_n_args, n.n.args)
  startDate = as.Date(format(date_decimal(head(zoo::index(x[[1]]),1)), "%Y-%m-%d"))

  if (class(x)!="list"){
    print("Put the data into a list and retry dummy")
    model_list <- NULL
    stop()
    } else {
      x <- lapply(x, window, start = decimal_date(startDate))
      cl <- makeCluster(getOption("cl.cores", num.cores))
      par_auto <- parLapply(cl,x,auto.arima, max.p = auto_args$max.p, max.q = auto_args$max.q, max.P = auto_args$max.P,
                            max.Q = auto_args$max.Q, max.order = auto_args$max.order, max.d = auto_args$max.d, max.D = auto_args$max.D,
                            start.p = auto_args$start.p, start.q = auto_args$start.q, start.P = auto_args$start.P, start.Q = auto_args$start.Q,
                            stationary = auto_args$stationary, seasonal = auto_args$seasonal, nmodels = auto_args$nmodels)
      par_ets <- parLapply(cl, x, ets, model = ets_args$model, damped = ets_args$damped, alpha = ets_args$alpha, beta = ets_args$beta,
                           gamma = ets_args$gamma, phi = ets_args$phi, additive.only = ets_args$additive.only, lambda = ets_args$lambda,
                           biasadj = ets_args$biasadj)
      par_tbats <- parLapply(cl, x, tbats, use.box.cox = tbats_args$use.box.cox, use.trend = tbats_args$use.trend, use.damped.trend = tbats_args$use.damped.trend,
                             seasonal.periods = tbats_args$seasonal.periods, use.arma.errors = tbats_args$use.arma.errors)
      par_hybrid <- parLapply(cl, x, hybridModel,  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      par_hybrid_in <- parLapply(cl, x, hybridModel, weights = "insample.errors",  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      date <- seq(ymd(format(date_decimal(head(zoo::index(x[[1]]),1)), "%Y-%m-%d")), ymd(format(date_decimal(tail(zoo::index(x[[1]]),1)), "%Y-%m-%d")), by = freq)
      prophet_setup <- lapply(x, as.data.frame)
      prophet_setup <- lapply(prophet_setup, cbind, ds = date)
      prophet_setup <- lapply(prophet_setup, rename, y = x)
      par_prophet <- lapply(prophet_setup, prophet, growth = prophet_args$growth, changepoints = prophet_args$changepoints, n.changepoints = prophet_args$n.changepoints,
                            changepoint.range = prophet_args$changepoint.range, yearly.seasonality = prophet_args$yearly.seasonality, weekly.seasonality = prophet_args$weekly.seasonality,
                            daily.seasonality = prophet_args$daily.seasonality, holidays = prophet_args$holidays, seasonality.mode = prophet_args$seasonality.mode)
      par_dlm <- parLapply(cl, x, dlmPara, dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps, freq = freq)
      model_list <- c(par_auto, par_ets, par_tbats, par_hybrid, par_hybrid_in, par_prophet, par_dlm)
      stopCluster(cl)
    }

  return(model_list)
}


#'DLM building function that models and forecast a dlm model for each TS in the par_catch_all cal
#'
#' @param x ts
#' @param dlmPoly polynomial order used in model.build
#' @param dlmSeas seasonal portion
#' @param steps steps ahead
#' @param freq "month"
#'
#' @return array of fitted + forecast
#' @export
#'
#' @examples test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#' test_forecast <- extract_model_fit_forecast(test_model, num.cores = 12)
dlmPara <- function ( x, dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps , freq = freq){
  model.build <- function(p) {
    return(
      dlm::dlmModPoly(dlmPoly, dV=p[1], dW=p[2:3]) +
        dlm::dlmModSeas(dlmSeas, dV=p[4])
    )
  }
  model.mle <- dlm::dlmMLE(x, parm=c(0.1, 0, 1, 1), build=model.build)
  model.fit <- model.build(model.mle$par)
  if (freq == "month"){
    time <- 12
  } else if (freq == "day") {
    time <- 365
  } else if (freq == "week") {
    time <- 52
  } else if (freq == "quarter") {
    time <- 4
  } else { time <- 1}
  model.filtered <- dlm::dlmFilter(x, model.fit)
  model.smoothed <- dlm::dlmSmooth(x, model.fit)
  model.forecast <- dlm::dlmForecast(model.filtered, nAhead=steps)
  xf <- seq(max(zoo::index(x)), max(zoo::index(x))+steps/time, 1/time)
  xf <- xf[(-1)]
  aa <- model.forecast$a[,-1]*(-1)
  aa <- cbind(model.forecast$a[,1], aa)
  a <- drop(model.forecast$a%*%t(dlm::FF(model.fit)))
  a <- c(x,a)
  class(a) <- "dlm"
  return(a)
}

#' Extract the numeric array of fitted values with the forecast added on to the end.
#'
#' @param x list of models from par_catch_all
#' @param steps integer of how many steps out you want to forecast
#' @param xreg list of matrix/array of exogenous regressors you want to use to forecast only applicable to certain models
#' @param freq string of characters indicating e.g. "month" for monthly etc
#' @param num.cores integer for the number of cores you want to use
#'
#' @return list of numeric arrays with forecasts appended
#' @export
#'
#' @examples  test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#' test_forecast <- extract_model_fit_forecast(test_model, num.cores = 12)
extract_model_fit_forecast <- function(x, steps = 3, xreg = NULL, freq = "month", num.cores = 2){
  a = list()
  b = list()
  c = list()
  hybrid = list()
  for (i in 1:(length(x)*(3/7))){
    a[[i]] <- x[[i]]
  }
  for (i in (length(x)*(3/7)+1):(length(x)*(5/7))){
    hybrid[[i]] <- x[[i]]
  }
  hybrid <- hybrid[lengths(hybrid) != 0]
  for (i in (length(x)*(5/7)+1):(length(x)*(6/7))){
    b[[i]] <- x[[i]]
  }
  b <- b[lengths(b) != 0]
  for (i in (length(x)*(6/7)+1):length(x)){
    c[[i]] <- x[[i]]
  }
  c <- c[lengths(c) != 0]
  cl <- makeCluster(getOption("cl.cores", num.cores))
  final_forecast <- parLapply(cl, a, forecast, h=steps, xreg=xreg)
  final_hybrid <- lapply(hybrid, forecast, h=steps, xreg=xreg)
  prophet_forecast <- function(x, period = 3, freq = "month"){
    future <- prophet::make_future_dataframe(x, period, freq)
    forecast <- predict(x, future)
    final <- forecast$yhat
    return(final)
  }
  final_proph <- parLapply(cl, b, prophet_forecast, period = steps, freq = freq)
  stopCluster(cl)
  final_dlm <- c
  forecast_combine <- c(final_forecast, final_hybrid)
  final_reg <- lapply(forecast_combine, function(x){c(x$fitted, x$mean)})
  final <- c(final_reg, final_proph, final_dlm)
  return(final)
}





