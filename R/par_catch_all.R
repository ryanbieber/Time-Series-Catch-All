## data goes into a list of dataframes
library(forecast)
library(parallel)
library(forecastHybrid)
library(dplyr)
library(prophet)
library(lubridate)

par_time_series_catch <- function(x, startDate = NULL, endDate = NULL, freq = 12, steps = 3, num.cores = 2, OutOfSample = FALSE, a.a.args = list(NULL),
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
  startDate = ymd(startDate)
  endDate = ymd(endDate)

  if (class(x)!="list"){
    print("Put the data into a list and retry dummy")
    model_list <- NULL
    break()}
  if (OutOfSample == FALSE & is.null(startDate)|is.null(endDate)){
        print("Need to have dates to proceed")
        break()}
  else if (OutOfSample == FALSE){
      x <- lapply(x, window, start = decimal_date(startDate), end = decimal_date(endDate))
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
      date <- seq(as.Date(startDate), as.Date(endDate), by = freq)
      prophet_setup <- lapply(x, as.data.frame)
      prophet_setup <- lapply(prophet_setup, cbind, ds=date)
      prophet_setup <- lapply(prophet_setup, rename, y=x)
      par_prophet <- lapply(prophet_setup, prophet, growth = prophet_args$growth, changepoints = prophet_args$changepoints, n.changepoints = prophet_args$n.changepoints,
                            changepoint.range = prophet_args$changepoint.range, yearly.seasonality = prophet_args$yearly.seasonality, weekly.seasonality = prophet_args$weekly.seasonality,
                            daily.seasonality = prophet_args$daily.seasonality, holidays = prophet_args$holidays, seasonality.mode = prophet_args$seasonality.mode)
      stopCluster(cl)
      model_list <- c(par_auto, par_ets, par_tbats, par_hybrid, par_hybrid_in, par_prophet)
    } else {
      if (OutOfSample == TRUE & is.null(startDate)|is.null(endDate)){
        print("You need to have a starting date and ending date for your training period")
        break()
      }
      x <- lapply(x, window, start = decimal_date(startDate), end = decimal_date(endDate))
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
      par_hybrid <- parLapply(cl, x, hybridModel, a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      par_hybrid_in <- parLapply(cl, x, hybridModel, weights = "insample.errors", a.args = auto_args, e.args = ets_args, t.args = tbats_args, n.args = n_n_args)
      date <- seq(as.Date(startDate), as.Date(endDate), by = freq)
      prophet_setup <- parLapply(cl, x, as.data.frame)
      prophet_setup <- parLapply(cl, prophet_setup, cbind, ds=date)
      prophet_setup <- parLapply(cl, prophet_setup, rename, y=x)
      par_prophet <- parLapply(cl, prophet_setup, prophet, growth = prophet_args$growth, changepoints = prophet_args$changepoints, n.changepoints = prophet_args$n.changepoints,
                            changepoint.range = prophet_args$changepoint.range, yearly.seasonality = prophet_args$yearly.seasonality, weekly.seasonality = prophet_args$weekly.seasonality,
                            daily.seasonality = prophet_args$daily.seasonality, holidays = prophet_args$holidays, seasonality.mode = prophet_args$seasonality.mode)
      stopCluster(cl)
      model_list <- c(par_auto, par_ets, par_tbats, par_hybrid, par_hybrid_in, par_prophet)
    }
  return(model_list)
}

hundo <- replicate(1, list(ldeaths))
test <- par_time_series_catch(hundo, num.cores = 12, startDate = "1974-01-01", endDate = "1979-12-01")



startDate = c(1974, 01)
endDate = c(1979, 12)
startDateS <- paste(startDate[1], ifelse(nchar(startDate[2])>1, startDate[2], paste0("0", startDate[2])), "01", sep = "-")
endDateS <- paste(endDate[1],ifelse(nchar(endDate[2])>1, endDate[2], paste0("0", endDate[2])), "01", sep = "-")
date <- seq(as.Date(startDateS), as.Date(endDateS), by = 365.25/7)


