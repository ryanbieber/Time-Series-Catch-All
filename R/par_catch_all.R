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
#' @param n.n.args list of arguments you want to change
#'
#' @return list of models for eacg ts
#' @export
#'
#' @examples \dontrun{
#' test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#'}
#'
par_time_series_catch <- function(original, freq = "month", steps = 3, dlmPoly = 2, dlmSeas = 12,  num.cores = 2, error = "mape", xreg = NULL, a.a.args = list(NULL),
                                  ets.args = list(NULL), tbats.args = list(NULL),  n.n.args = list(NULL)){
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
  n_n_args <- list(P = 1, repeats = 20, xreg = NULL, lambda = NULL,
                   model = NULL, subset = NULL, scale.inputs = TRUE)

  ## modifying lists to update code
  auto_args <- utils::modifyList(auto_args, a.a.args)
  ets_args <- utils::modifyList(ets_args, ets.args)
  tbats_args <- utils::modifyList(tbats_args, tbats.args)
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
    ##modelling functions
    cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
    par_auto <- parallel::parLapply(cl,x,forecast::auto.arima, max.p = auto_args$max.p, max.q = auto_args$max.q, max.P = auto_args$max.P,
                                    max.Q = auto_args$max.Q, max.order = auto_args$max.order, max.d = auto_args$max.d, max.D = auto_args$max.D,
                                    start.p = auto_args$start.p, start.q = auto_args$start.q, start.P = auto_args$start.P, start.Q = auto_args$start.Q,
                                    stationary = auto_args$stationary, seasonal = auto_args$seasonal, nmodels = auto_args$nmodels)
    parallel::stopCluster(cl)
    cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
    par_ets <- parallel::parLapply(cl, x, forecast::ets, model = ets_args$model, damped = ets_args$damped, alpha = ets_args$alpha, beta = ets_args$beta,
                                   gamma = ets_args$gamma, phi = ets_args$phi, additive.only = ets_args$additive.only, lambda = ets_args$lambda,
                                   biasadj = ets_args$biasadj)
    parallel::stopCluster(cl)
    cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
    par_tbats <- parallel::parLapply(cl, x, forecast::tbats, use.box.cox = tbats_args$use.box.cox, use.trend = tbats_args$use.trend, use.damped.trend = tbats_args$use.damped.trend,
                                     seasonal.periods = tbats_args$seasonal.periods, use.arma.errors = tbats_args$use.arma.errors)
    parallel::stopCluster(cl)
    cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
    par_hybrid <- parallel::parLapply(cl, x, forecastHybrid::hybridModel,  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
    parallel::stopCluster(cl)
    cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
    par_dlm <- parallel::parLapply(cl, x, dlmPara, dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps, freq = freq)
    parallel::stopCluster(cl)
    model_list <- c(par_auto, par_ets, par_tbats, par_hybrid, par_dlm)
  }
  print("Modelling Done")
  model_forecast <- extract_model_fit_forecast(model_list, steps = steps, xreg = xreg, freq = freq, num.cores = num.cores)
  print("Forecasting Done")
  best_model <- best_forecast_type(model_forecast, steps = steps, origin = original)
  print("Picking Best Model")
  if (error=="mape"){
    error_df <- best_model[[2]]
  } else if (error=="smape"){
    error_df <- best_model[[1]]
  } else if (error=="mase"){
    error_df <- best_model[[3]]
  } else {
    print("Not a valid error metric")
    stop()
  }

  models <- colnames(error_df)
  orig_x <- lapply(original, stats::window, start = lubridate::decimal_date(startDate))
  cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
  models_final <- parallel::parLapply(cl, orig_x, forecast_function, models, freq, steps, dlmPoly, dlmSeas, num.cores, error, xreg, a.a.args,
                                      ets.args, tbats.args, n.n.args)
  parallel::stopCluster(cl)
  print("Forecasting with Best Model")
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
  ## forecasts are put into lists based on forecasting type
  aa_ets_tbats = list()
  dlm = list()
  hybrid = list()
  for (i in 1:(length(x)*(3/5))){
    aa_ets_tbats[[i]] <- x[[i]]
  }
  for (i in (length(x)*(3/5)+1):(length(x)*(4/5))){
    hybrid[[i]] <- x[[i]]
  }
  hybrid <- hybrid[lengths(hybrid) != 0]
  for (i in (length(x)*(4/5)+1):length(x)){
    dlm[[i]] <- x[[i]]
  }
  dlm <- dlm[lengths(dlm) != 0]
  cl <- parallel::makeCluster(getOption("cl.cores", num.cores))
  final_forecast <- parallel::parLapply(cl, aa_ets_tbats, forecast::forecast, h=steps, xreg=xreg)
  parallel::stopCluster(cl)
  final_hybrid <- lapply(hybrid, forecast::forecast, h=steps, xreg=xreg, PI=FALSE)
  final_dlm <- dlm
  forecast_combine <- c(final_forecast, final_hybrid)
  final_reg <- lapply(forecast_combine, function(x){c(x$x, x$mean)})
  final <- c(final_reg, final_dlm)
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
    colnames(first_ts) <- c("Auto Arima", "ETS", "TBATS", "Hybrid", "Hybrid-In", "DLM", "Original")
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

#' forecast function that forecasts out how many steps you look forward
#'
#' @param orig_x list of time series you want to forecast
#' @param models list of models
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
#' @param n.n.args list of arguments you want to change
#'
#' @return list of models for models
#' @export
#'
#' @examples \dontrun{
#' test <- replicate(5, list(ldeaths))
#' test_model <- par_time_series_catch(test, num.cores = 12)
#'}
#'
forecast_function <- function(orig_x, models, freq = freq, steps = steps, dlmPoly = dlmPoly, dlmSeas = dlmSeas,  num.cores = num.cores, error = error, xreg = xreg, a.a.args = a.a.args,
                              ets.args = ets.args, tbats.args = tbats.args, n.n.args = n.n.args) {
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
  n_n_args <- list(P = 1, repeats = 20, xreg = NULL, lambda = NULL,
                   model = NULL, subset = NULL, scale.inputs = TRUE)
  auto_args <- utils::modifyList(auto_args, a.a.args)
  ets_args <- utils::modifyList(ets_args, ets.args)
  tbats_args <- utils::modifyList(tbats_args, tbats.args)
  n_n_args <- utils::modifyList(n_n_args, n.n.args)
  i <- parent.frame()$i[]

  models <- models[i]

  if ( grepl("Auto Arima",models)){
    model <- forecast::auto.arima(orig_x, max.p = auto_args$max.p, max.q = auto_args$max.q, max.P = auto_args$max.P,
                                  max.Q = auto_args$max.Q, max.order = auto_args$max.order, max.d = auto_args$max.d, max.D = auto_args$max.D,
                                  start.p = auto_args$start.p, start.q = auto_args$start.q, start.P = auto_args$start.P, start.Q = auto_args$start.Q,
                                  stationary = auto_args$stationary, seasonal = auto_args$seasonal, nmodels = auto_args$nmodels)
    forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
    forecast_model <- c(forecast_model$x, forecast_model$mean)
  } else if ( grepl("ETS",models)){
    model <- forecast::ets(orig_x, model = ets_args$model, damped = ets_args$damped, alpha = ets_args$alpha, beta = ets_args$beta,
                           gamma = ets_args$gamma, phi = ets_args$phi, additive.only = ets_args$additive.only, lambda = ets_args$lambda,
                           biasadj = ets_args$biasadj)
    forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
    forecast_model <- c(forecast_model$x, forecast_model$mean)
  } else if (grepl("TBATS",models)){
    model <- forecast::tbats(orig_x, use.box.cox = tbats_args$use.box.cox, use.trend = tbats_args$use.trend, use.damped.trend = tbats_args$use.damped.trend,
                             seasonal.periods = tbats_args$seasonal.periods, use.arma.errors = tbats_args$use.arma.errors)
    forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
    forecast_model <- c(forecast_model$x, forecast_model$mean)
  } else if ( grepl("Hybrid",models)){
    model <- forecastHybrid::hybridModel(orig_x ,  a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
    forecast_model <- forecast::forecast(model, h=steps, xreg=xreg)
    forecast_model <- c(forecast_model$x, forecast_model$mean)
  } else {
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
    forecast_model <- dlmPara(orig_x,  dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps, freq = freq)

  }
  return(forecast_model)
}
