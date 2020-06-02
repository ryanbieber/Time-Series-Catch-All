## data goes into a list of dataframes

#' The big dawg that will forecast with the best of them
#'
#' @param original list of time series you want to forecast
#' @param freq character string indicating "month" for monthly etc.
#' @param steps integer that explains how far out you want forecast(needed for DLM)
#' @param dlmPoly integer for the order of polynomial you want in dlm, default = 2
#' @param dlmSeas integer for seasonal effect in dlm, default is 12 for monthly
#' @param num.cores integer for the number of cores you want to use
#' @param error either is "smape" or "mape", default is "mape"
#' @param xreg list of matrix/array of exogenous regressors you want to use to forecast only applicable to certain models
#' @param a.a.args list of arguments you want to change
#' @param ets.args list of arguments you want to change
#' @param tbats.args list of arguments you want to change
#' @param prophet.args list of arguments you want to change
#' @param n.n.args list of arguments you want to change
#' @return list of models for eacg ts
#' @export
#'
#' @examples \dontrun{
#' test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#'}
#'
par_time_series_catch <- function(original, freq = "month", steps = 3, dlmPoly = 2, dlmSeas = 12,  num.cores = 2, error = "mape", xreg = NULL, a.a.args = list(NULL),
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
  auto_args <- utils::modifyList(auto_args, a.a.args)
  ets_args <- utils::modifyList(ets_args, ets.args)
  tbats_args <- utils::modifyList(tbats_args, tbats.args)
  prophet_args <- utils::modifyList(prophet_args, prophet.args)
  n_n_args <- utils::modifyList(n_n_args, n.n.args)
  startDate = zoo::as.Date(format(lubridate::date_decimal(utils::head(zoo::index(original[[1]]),1)), "%Y-%m-%d"))

  if (class(original)!="list"){
    print("Put the data into a list and retry dummy")
    model_list <- NULL
    stop()
    } else {
      x <- lapply(original, utils::head, -steps)
      x <- lapply(x, stats::as.ts)
      non_ts <- unlist(lapply(x, stats::is.ts))
      if (FALSE %in% non_ts){
        print("Your list contains non time-series, please change them all to time-series")
        stop()
      }
      #x <- lapply(x, stats::window, start = lubridate::decimal_date(startDate))
      cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
      par_auto <- parallel::parLapply(cl,x,forecast::auto.arima, max.p = auto_args$max.p, max.q = auto_args$max.q, max.P = auto_args$max.P,
                            max.Q = auto_args$max.Q, max.order = auto_args$max.order, max.d = auto_args$max.d, max.D = auto_args$max.D,
                            start.p = auto_args$start.p, start.q = auto_args$start.q, start.P = auto_args$start.P, start.Q = auto_args$start.Q,
                            stationary = auto_args$stationary, seasonal = auto_args$seasonal, nmodels = auto_args$nmodels)
      par_ets <- parallel::parLapply(cl, x, forecast::ets, model = ets_args$model, damped = ets_args$damped, alpha = ets_args$alpha, beta = ets_args$beta,
                           gamma = ets_args$gamma, phi = ets_args$phi, additive.only = ets_args$additive.only, lambda = ets_args$lambda,
                           biasadj = ets_args$biasadj)
      par_tbats <- parallel::parLapply(cl, x, forecast::tbats, use.box.cox = tbats_args$use.box.cox, use.trend = tbats_args$use.trend, use.damped.trend = tbats_args$use.damped.trend,
                             seasonal.periods = tbats_args$seasonal.periods, use.arma.errors = tbats_args$use.arma.errors)
      par_hybrid <- parallel::parLapply(cl, x, forecastHybrid::hybridModel,  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      par_hybrid_in <- parallel::parLapply(cl, x, forecastHybrid::hybridModel, weights = "insample.errors",  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      date <- seq(lubridate::ymd(format(lubridate::date_decimal(utils::head(zoo::index(x[[1]]),1)), "%Y-%m-%d")),
                  lubridate::ymd(format(lubridate::date_decimal(utils::tail(zoo::index(x[[1]]),1)), "%Y-%m-%d")), by = freq)
      prophet_setup <- lapply(x, as.data.frame)
      prophet_setup <- lapply(prophet_setup, base::cbind, ds = date)
      prophet_setup <- lapply(prophet_setup, dplyr::rename, y = x)
      par_prophet <- lapply(prophet_setup, prophet::prophet, growth = prophet_args$growth, changepoints = prophet_args$changepoints, n.changepoints = prophet_args$n.changepoints,
                            changepoint.range = prophet_args$changepoint.range, yearly.seasonality = prophet_args$yearly.seasonality, weekly.seasonality = prophet_args$weekly.seasonality,
                            daily.seasonality = prophet_args$daily.seasonality, holidays = prophet_args$holidays, seasonality.mode = prophet_args$seasonality.mode)
      par_dlm <- parallel::parLapply(cl, x, dlmPara, dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps, freq = freq)
      model_list <- c(par_auto, par_ets, par_tbats, par_hybrid, par_hybrid_in, par_prophet, par_dlm)
      parallel::stopCluster(cl)
    }


  model_forecast <- extract_model_fit_forecast(model_list, steps = steps, xreg = xreg, freq = freq, num.cores = num.cores)

  best_model <- best_forecast_type(model_forecast, steps = steps, origin = original)

  if (error=="mape"){
    error_df <- best_model[[2]]
  } else if (error=="smape"){
    error_df <- best_model[[1]]
  } else {
    print("Not a valid error metric")
    stop()
  }

  models <- colnames(error_df)
  x <- lapply(original, stats::window, start = lubridate::decimal_date(startDate))

  models_final <- list()
  for (i in 1:length(models)){
    if ( grepl("Auto Arima",models[i])){
      model <- forecast::auto.arima(x[[i]], max.p = auto_args$max.p, max.q = auto_args$max.q, max.P = auto_args$max.P,
                                    max.Q = auto_args$max.Q, max.order = auto_args$max.order, max.d = auto_args$max.d, max.D = auto_args$max.D,
                                    start.p = auto_args$start.p, start.q = auto_args$start.q, start.P = auto_args$start.P, start.Q = auto_args$start.Q,
                                    stationary = auto_args$stationary, seasonal = auto_args$seasonal, nmodels = auto_args$nmodels)
      forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
      forecast_model <- c(forecast_model$fitted, forecast_model$mean)
    } else if ( grepl("ETS",models[i])){
      model <- forecast::ets(x[[i]], model = ets_args$model, damped = ets_args$damped, alpha = ets_args$alpha, beta = ets_args$beta,
                             gamma = ets_args$gamma, phi = ets_args$phi, additive.only = ets_args$additive.only, lambda = ets_args$lambda,
                             biasadj = ets_args$biasadj)
      forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
      forecast_model <- c(forecast_model$fitted, forecast_model$mean)
    } else if (grepl("TBATS",models[i])){
      model <- forecast::tbats(x[[i]], use.box.cox = tbats_args$use.box.cox, use.trend = tbats_args$use.trend, use.damped.trend = tbats_args$use.damped.trend,
                               seasonal.periods = tbats_args$seasonal.periods, use.arma.errors = tbats_args$use.arma.errors)
      forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
      forecast_model <- c(forecast_model$fitted, forecast_model$mean)
    } else if ( grepl("Hybrid",models[i])){
      model <- forecastHybrid::hybridModel(x[[i]] ,  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
      forecast_model <- c(forecast_model$fitted, forecast_model$mean)
    } else if ( grepl("Hybrid-In",models[i])){
      model <- forecastHybrid::hybridModel(x[[i]] , weights = "insample.errors",  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
      forecast_model <- c(forecast_model$fitted, forecast_model$mean)
    } else if ( grepl("Prophet",models[i])){
      date <- seq(lubridate::ymd(format(lubridate::date_decimal(utils::head(zoo::index(x[[1]]),1)), "%Y-%m-%d")),
                  lubridate::ymd(format(lubridate::date_decimal(utils::tail(zoo::index(x[[1]]),1)), "%Y-%m-%d")), by = freq)
      prophet_setup <- as.data.frame(x[[i]])
      prophet_setup <- cbind(prophet_setup, ds = date)
      prophet_setup <- dplyr::rename(prophet_setup, y = x)
      model <- prophet::prophet(prophet_setup,  growth = prophet_args$growth, changepoints = prophet_args$changepoints, n.changepoints = prophet_args$n.changepoints,
                                changepoint.range = prophet_args$changepoint.range, yearly.seasonality = prophet_args$yearly.seasonality, weekly.seasonality = prophet_args$weekly.seasonality,
                                daily.seasonality = prophet_args$daily.seasonality, holidays = prophet_args$holidays, seasonality.mode = prophet_args$seasonality.mode)
      prophet_forecast <- function(x, period = steps, freq = "month"){
        future <- prophet::make_future_dataframe(x, period, freq)
        forecastp <- stats::predict(x, future)
        final <- forecastp$yhat
        return(final)
      }
      forecast_model <- prophet_forecast(model, period = steps, freq = freq)
    } else {
      forecast_model <- dlmPara(x[[i]],  dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps, freq = freq)

    }
    models_final[[i]] <- forecast_model

  }

  best_model_forecast <- dplyr::bind_cols(models_final)
  colnames(best_model_forecast) <- models

  return(best_model_forecast)
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
#' @examples \dontrun{
#' test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#' }


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
#' @return list of numeric arrays with forecasts appended
#' @export
#'
#' @examples \dontrun{
#' test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#' }

#'
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
  cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
  final_forecast <- parallel::parLapply(cl, a, forecast::forecast, h=steps, xreg=xreg)
  final_hybrid <- lapply(hybrid, forecast::forecast, h=steps, xreg=xreg)
  prophet_forecast <- function(x, period = steps, freq = "month"){
    future <- prophet::make_future_dataframe(x, period, freq)
    forecastp <- stats::predict(x, future)
    final <- forecastp$yhat
    return(final)
  }
  final_proph <- parallel::parLapply(cl, b, prophet_forecast, period = steps, freq = freq)
  parallel::stopCluster(cl)
  final_dlm <- c
  forecast_combine <- c(final_forecast, final_hybrid)
  final_reg <- lapply(forecast_combine, function(x){c(x$fitted, x$mean)})
  final <- c(final_reg, final_proph, final_dlm)

  return(final)
}



#' Picks the best model based on mape and smape and outputs a list
#'
#' @param model_forecast output from extract_model_fit_forecast
#' @param steps numeric based on how far you want to look back and ahead
#' @param origin series you are comparing agianst
#'
#' @return list of data frames with mape and smape models
#' @export
#'
#' @examples \dontrun{
#' test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#' }
#'
best_forecast_type <- function(model_forecast, steps = steps, origin = NULL){
  forecasts <- length(model_forecast)/7
  best_mape <- list()
  best_smape <- list()
  for (q in 1:forecasts){
    seq_list <- seq(q, length(model_forecast), by = forecasts)
    first_ts <- cbind(dplyr::bind_cols(model_forecast[seq_list]), as.numeric(origin[[q]]))
    colnames(first_ts) <- c("Auto Arima", "ETS", "TBATS", "Hybrid", "Hybrid-In", "Prophet", "DLM", "Original")
    mape <- NA
    smape <- NA
    for (i in 1:7){
      mape[i] <- Metrics::mape(utils::tail(as.numeric(first_ts$Original),steps), utils::tail(as.numeric(first_ts[,i]),steps))
      smape[i] <- Metrics::smape(utils::tail(as.numeric(first_ts$Original),steps), utils::tail(as.numeric(first_ts[,i]),steps))
    }

    minmape <- match(min(mape), mape)
    best_forecastmape <- as.data.frame(as.double(first_ts[,minmape]))
    colnames(best_forecastmape) <- paste(c(names(first_ts)[minmape]), q, sep =", " )
    best_mape[[q]] <- best_forecastmape

    minsmape <- match(min(smape), smape)
    best_forecastsmape <- as.data.frame(as.double(first_ts[,minsmape]))
    colnames(best_forecastsmape) <-  paste(c(names(first_ts)[minsmape]), q, sep =", " )
    best_smape[[q]] <- best_forecastsmape
  }

  smape_df <- dplyr::bind_cols(best_smape)
  mape_df <- dplyr::bind_cols(best_mape)

  error_list <- list("smape" = smape_df, "mape" = mape_df)
  return(error_list)
}

