#' ONLY USE IF YOU ALREADY RAN THE FIRST MODEL(time_series_catch_all) AND HAVE A VEcTOR OF MODEL NAMES
#'
#' @param data original data with Date in first column
#' @param f frequency of data i.e. month/day/year 12/365/1, interger
#' @param h forecasting steps up to 3, integer
#' @param trainStart numeric vector, i.e. c(2016, 1) for Jan 2016
#' @param trainEnd numeric vector, i.e. c(2016, 1) for Jan 2016
#' @param test numeric vector, i.e. c(2016, 1) for Jan 2016
#' @param n integer, number of cores you want to use
#' @param is.VL if the data is vended labor flag it with is.VL = TRUE
#' @param seasonaility if quarterly choose seasonaility = "Quarter" otherwise dont change default is monthly
#' @param bestModels vector of best modelNames, comes from best_model_name function
#'
#' @return a list of data frames that only contain the best model found by using the best_model_name function
#' @export
#'
#' @examples the fast version of time-series catch all, can oly be run after you run the first model due to the dependency on the bestModels variable
#' as it will only run those types of models in the future. To use this you need to take the output from names_of_best_model and input it into the
#' bestModels variable. therefore it will only run the functions that were chosen to be the best model.
fast_time_series_catch_all <- function(data, f, h, trainStart, trainEnd, test, n, bestModels, is.VL = FALSE, seasonaility = "Monthly"){
  # initializing list
  cols <- colnames(data)
  DF_Fast=list()
  # xts timeseries
  ts_data <- xts(data, order.by = data$Date)
  ts_data<- t(ts_data)
  ts_data<- tslist(ts_data)

  if (is.VL == TRUE){
    xreg <- exogenous_regressors(ts_data)
  } else{}


    for (i in 2:length(data)){  #goes through all columns assume col 1 is the date field and skipping that one
      ## making the train starting date dynamic as some columns have missing data at the beginning
      frame <- ts_data[[1]]
      na <- sum(is.na(ts_data[[i]]))
      frame <- frame[na+1]
      year <- as.numeric(substr(frame, 1, 4))
      month <- as.numeric(substr(frame, 6, 7))

      ## formatting time series for other packages
      #setting up the main time series list that will be used in the rest of the models
      time <-ts(ts_data[[i]], frequency = f, start = trainStart)
      n_train<-window(time , start = c(year, month), end = trainEnd) #training columns
      n_test<-window(time ,start = test) #testing columns

      if (bestModels[i-1]=="Prophet"){
        #running facebooks prophet model and setting up a certain data frame as it is picky in how it likes the inputs
        Z <- ts_prophet(data, h, i)

      } else if (bestModels[i-1]=="DLM"){
        #dlm model
        Z <- ts_dlm_model(time, n_train, h, seasonaility)
        Z <- as.numeric(n_train)
        Z <- c(a1, a)

      } else if (bestModels[i-1]=="ETS"){
        #exponntial smoothing
        ETSe <- ets(as.double(n_train))
        Zf <- fitted.values(ETSe)
        Z <- forecast(ETSe, h=h)
        Z<- ts(Z$mean,
                 frequency = f, start = test)
        Zf <- as.numeric(Zf)
        Z <- as.numeric(Z)
        Z <- c(Zf, Z)

      } else if (bestModels[i-1]=="ARIMAX"){
        if (is.VL == TRUE){
          #setting up xregs for ARIMA and Hybridmodel
          xregtrain <- xreg[c(1:(NROW(n_train))),1]
          xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
          xregnntrain <- xreg[c(1:(NROW(n_train))),]
          xregnntest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),]

          ## if you go to 1 forecasted value it messes up the format of multiple external regressors
          if (h==1){
            xregnntest <- t(as.matrix(xregnntest))
            Zf <- as.numeric(Zf)
            Z <- as.numeric(Z)
            Z <- c(Zf, Z)
          }

          #arima modelling
          ARIMAa <- auto.arima(as.double(n_train),
                               stepwise = FALSE, parallel = TRUE, xreg = xregtrain, biasadj = TRUE, ic ="aicc")
          Zf <- fitted.values(ARIMAa)
          Z <- forecast(ARIMAa,
                            h=h, xreg = xregtest)
          Z<- ts(Z$mean,
                     frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        } else {
          #arima modelling
          ARIMAa <- auto.arima(as.double(n_train),
                               stepwise = FALSE, parallel = TRUE, biasadj = TRUE, ic ="aicc")
          Zf <- fitted.values(ARIMAa)
          Z <- forecast(ARIMAa,
                            h=h)
          Z<- ts(Z$mean,
                 frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)
        }


      } else if (bestModels[i-1]=="TBATS"){
        #uses tbats model
        #uses tbats model
        TBATSt <- tbats(as.double(n_train),
                        lambda=0, use.parallel = TRUE, num.cores = n)
        Zf <- fitted.values(TBATSt)
        TBATS <- forecast(TBATSt,
                          h=h)
        Z<- ts(TBATS$mean,
                   frequency = f, start = test)
        Zf <- as.numeric(Zf)
        Z <- as.numeric(Z)
        Z <- c(Zf, Z)

      } else if (bestModels[i-1]=="HybridE"){
        #setting N-train to numeric as hybridModel needs it like that
        n_train<-as.numeric(n_train)
        if (is.VL == TRUE){
          #setting up xregs for ARIMA and Hybridmodel
          xregtrain <- xreg[c(1:(NROW(n_train))),1]
          xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,
                                  n.args = list(xreg = xregtrain),
                                  a.args = list(xreg = xregtrain))
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h, xreg = xregtest) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        } else {

          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,)
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        }


      } else if (bestModels[i-1]=="HybridIn") {
        #setting N-train to numeric as hybridModel needs it like that
        n_train<-as.numeric(n_train)
        if (is.VL == TRUE){
          #setting up xregs for ARIMA and Hybridmodel
          xregtrain <- xreg[c(1:(NROW(n_train))),1]
          xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,
                                  n.args = list(xreg = xregtrain),
                                  a.args = list(xreg = xregtrain))
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h, xreg = xregtest) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        } else {

          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,)
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        }


      } else if (bestModels[i-1]=="HybridENN"){
        #setting N-train to numeric as hybridModel needs it like that
        n_train<-as.numeric(n_train)
        if (is.VL == TRUE){
          #setting up xregs for ARIMA and Hybridmodel
          xregtrain <- xreg[c(1:(NROW(n_train))),1]
          xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,
                                  n.args = list(xreg = xregtrain))
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h, xreg = xregtest) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        } else {

          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,)
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        }


      } else if (bestModels[i-1]=="HybridInNN"){
        #setting N-train to numeric as hybridModel needs it like that
        n_train<-as.numeric(n_train)
        if (is.VL == TRUE){
          #setting up xregs for ARIMA and Hybridmodel
          xregtrain <- xreg[c(1:(NROW(n_train))),1]
          xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,
                                  n.args = list(xreg = xregtrain))
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h, xreg = xregtest) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        } else {

          Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,)
          Zf <- fitted.values(Hybrid1h)
          Hybrid1 <- forecast(Hybrid1h,
                              h=h) # ?forecasthybrid
          Z <-ts(Hybrid1$mean, frequency = f, start = test)
          Zf <- as.numeric(Zf)
          Z <- as.numeric(Z)
          Z <- c(Zf, Z)

        }

      } else if (bestModels[i-1]=="Mixture"){
        if (is.VL == TRUE){
        #running facebooks prophet model and setting up a certain data frame as it is picky in how it likes the inputs
        fcastprophet <- ts_prophet(data, h, i)


        ## formatting time series for other packages
        #setting up the main time series list that will be used in the rest of the models
        time <-ts(ts_data[[i]], frequency = f, start = trainStart)
        n_train<-window(time , start = c(year, month), end = trainEnd) #training columns
        n_test<-window(time ,start = test) #testing columns
        #h <- length(n_test) # may need to be changed if you want to forecast using all the the data as training

        #dlm model
        a <- ts_dlm_model(time, n_train, h, seasonaility)


        #exponntial smoothing
        ETS <- forecast(ets(as.double(n_train)), h=h)

        #setting up xregs for ARIMA and Hybridmodel
        xregtrain <- xreg[c(1:(NROW(n_train))),1]
        xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
        xregnntrain <- xreg[c(1:(NROW(n_train))),]
        xregnntest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),]

        ## if you go to 1 forecasted value it messes up the format of multiple external regressors
        if (h==1){
          xregnntest <- t(as.matrix(xregnntest))
        }


        #arima modelling
        ARIMA <- forecast(auto.arima(as.double(n_train),
                                     stepwise = FALSE, parallel = TRUE, xreg = xregtrain, biasadj = TRUE, ic ="aicc"),
                          h=h, xreg = xregtest)

        #uses tbats model
        TBATS <- forecast(tbats(as.double(n_train),
                                lambda=0, use.parallel = TRUE, num.cores = n),
                          h=h)

        #gets the forecast values
        ETS<- ts(ETS$mean,
                 frequency = f, start = test)
        ARIMA<- ts(ARIMA$mean,
                   frequency = f, start = test)
        TBATS<- ts(TBATS$mean,
                   frequency = f, start = test)

        #setting N-train to numeric as hybridModel needs it like that
        n_train<-as.numeric(n_train)

        Hybrid1 <- forecast(hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,
                                        n.args = list(xreg = xregtrain),
                                        a.args = list(xreg = xregtrain)),
                            h=h, xreg = xregtest) # ?forecasthybrid

        Hybrid2 <- forecast(hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,
                                        n.args = list(xreg = xregtrain),
                                        a.args = list(xreg = xregtrain)),
                            h=h, xreg = xregtest)

        Hybrid3 <- forecast(hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,
                                        n.args = list(xreg = xregnntrain)),
                            h=h, xreg = xregnntest)

        Hybrid4 <- forecast(hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,
                                        n.args = list(xreg = xregnntrain)),
                            h=h, xreg = xregnntest)

        Hybrid1 <-ts(Hybrid1$mean, frequency = f, start = test)
        Hybrid2 <-ts(Hybrid2$mean, frequency = f, start = test)
        Hybrid3 <-ts(Hybrid3$mean, frequency = f, start = test)
        Hybrid4 <-ts(Hybrid4$mean, frequency = f, start = test)

        # comibining all columns to a giant data frame of forecasted values
        X <- cbind( ETS=ETS,
                    ARIMA=ARIMA,
                    TBATS=TBATS,
                    Hybrid1,
                    Hybrid2,
                    Hybrid3,
                    Hybrid4,
                    Prophet=fcastprophet,
                    DLM=a)

        # using ?opera package to ensamble the models together and pick the best one best off square loss loss type and model are changeable
        MLpol0 <- mixture(model = "MLpol", loss.type = "square")
        Z <- ts(predict(MLpol0, X, as.numeric(n_test),
                        type='response'),
                start=test, freq=f)

        Z <- ts(Z,
                frequency = f, start = test)
        Z <- as.numeric(Z)
        Z <- c(n_train, Z)

        }
      } else {
        #running facebooks prophet model and setting up a certain data frame as it is picky in how it likes the inputs
        fcastprophet <- ts_prophet(data, h, i)


        ## formatting time series for other packages
        #setting up the main time series list that will be used in the rest of the models
        time <-ts(ts_data[[i]], frequency = f, start = trainStart)
        n_train<-window(time , start = c(year, month), end = trainEnd) #training columns
        n_test<-window(time ,start = test) #testing columns
        #h <- length(n_test) # may need to be changed if you want to forecast using all the the data as training

        #dlm model
        a <- ts_dlm_model(time, n_train, h, seasonaility)


        #exponntial smoothing
        ETS <- forecast(ets(as.double(n_train)), h=h)


        #arima modelling
        ARIMA <- forecast(auto.arima(as.double(n_train),
                                     stepwise = FALSE, parallel = TRUE, biasadj = TRUE, ic ="aicc"),
                          h=h)

        #uses tbats model
        TBATS <- forecast(tbats(as.double(n_train),
                                lambda=0, use.parallel = TRUE, num.cores = n),
                          h=h)

        #gets the forecast values
        ETS<- ts(ETS$mean,
                 frequency = f, start = test)
        ARIMA<- ts(ARIMA$mean,
                   frequency = f, start = test)
        TBATS<- ts(TBATS$mean,
                   frequency = f, start = test)

        #setting N-train to numeric as hybridModel needs it like that
        n_train<-as.numeric(n_train)

        Hybrid1 <- forecast(hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n),
                            h=h) # ?forecasthybrid

        Hybrid2 <- forecast(hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n),
                            h=h)

        Hybrid3 <- forecast(hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n),
                            h=h)

        Hybrid4 <- forecast(hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n),
                            h=h)

        Hybrid1 <-ts(Hybrid1$mean, frequency = f, start = test)
        Hybrid2 <-ts(Hybrid2$mean, frequency = f, start = test)
        Hybrid3 <-ts(Hybrid3$mean, frequency = f, start = test)
        Hybrid4 <-ts(Hybrid4$mean, frequency = f, start = test)

      # comibining all columns to a giant data frame of forecasted values
      X <- cbind( ETS=ETS,
                  ARIMA=ARIMA,
                  TBATS=TBATS,
                  Hybrid1,
                  Hybrid2,
                  Hybrid3,
                  Hybrid4,
                  Prophet=fcastprophet,
                  DLM=a)

      # using ?opera package to ensamble the models together and pick the best one best off square loss loss type and model are changeable
      MLpol0 <- mixture(model = "MLpol", loss.type = "square")
      Z <- ts(predict(MLpol0, X, as.numeric(n_test),
                      type='response'),
              start=test, freq=f)

      Z <- ts(Z,
              frequency = f, start = test)
      Z <- as.numeric(Z)
      Z <- c(n_train, Z)
      }

      final <- cbind(time, Z)
      colnames(final) <- c(cols[i-1], bestModels[i-1])
      DF_Fast[[i]] <- final
      print(i)
  }

  return(DF_Fast)

}

