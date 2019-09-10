
#' time series catch all
#'
#' @param data original data with Date in first column
#' @param f integer
#' @param h integer
#' @param trainStart numeric vector
#' @param trainEnd numeric vector
#' @param test numeric vector
#' @param n integer
#' @param cols character vector
#'
#' @return a list of data frames with all the forecasts in each frame for each respective column
#' @export
#'
#' @examples # Function call for time series catch all with output DF is a list of forecasted fata frames
#' DF <- time_series_catch(data, f, h, trainStart, trainEnd, test, n, as.vector(unique)) in which data is the orignal data with the variable Date in the first column
#' f is frequency, h is forecasted steps up to 3, trainStart/trainEnd/test are all date vectors specified with c(2016, 1), n is the number of cores to use for pp
#' cols is a vector of equal length of the data column minus 1 which holds the names of all the unique column names.

time_series_catch <- function(data,  f, h, trainStart, trainEnd, test, n, cols){
  DF=list() #making a list of Data frames
  xreg <- exogenous_regressors(ts_data)
  ts_data <- xts(data, order.by = data$Date)
  ts_data<- t(ts_data)
  ts_data<- tslist(ts_data)
  #main loop that does all the modelling/ forecasting
  for (i in 2:length(data)){  #goes through all columns assume col 1 is the date field and skipping that one

    ## making the train starting date dynamic as some columns have missing data at the beginning
    frame <- ts_data[[1]]
    na <- sum(is.na(ts_data[[i]]))
    frame <- frame[na+1]
    year <- as.numeric(substr(frame, 1, 4))
    month <- as.numeric(substr(frame, 6, 7))



    #running facebooks prophet model and setting up a certain data frame as it is picky in how it likes the inputs
    fcastprophet <- ts_prophet(data, h, i)


    ## formatting time series for other packages
    #setting up the main time series list that will be used in the rest of the models
    time <-ts(ts_data[[i]], frequency = f, start = trainStart)
    n_train<-window(time , start = c(year, month), end = trainEnd) #training columns
    n_test<-window(time ,start = test) #testing columns
    #h <- length(n_test) # may need to be changed if you want to forecast using all the the data as training

    #dlm model
    a <- ts_dlm_model(time, n_train, h)


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

    #print(xregnntest)
    #break()

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

    #combining original and forecast values together
    Y <- cbind(time, X, Z)
    #print(Y)

    #rename columns for completness
    colnames(Y) <- c(cols[i-1],
                     "ETS",
                     "ARIMAX",
                     "TBATS",
                     "HybridE",
                     "HybridIn",
                     "HybridENN",
                     "HybridInNN",
                     "Prophet",
                     "DLM",
                     "Mixture")

    #appending them to an iterated list
    DF[[i]]<-Y
    print(i)  #making sure its loop correctly
  }
  return(DF)
}
