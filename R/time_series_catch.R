
#' time series catch all
#'
#' @param data original data with Date in first column
#' @param f frequency of data i.e. month/day/year 12/365/1, interger
#' @param h forecasting steps up to 3, integer
#' @param trainStart numeric vector, i.e. c(2016, 1) for Jan 2016 start of training period
#' @param trainEnd numeric vector, i.e. c(2016, 1) for Jan 2016 end of training period
#' @param test numeric vector, i.e. c(2016, 1) for Jan 2016 start of testing period
#' @param n integer, number of cores you want to use
#' @param is.XREG if the data is vended labor flag it with is.XREG = TRUE otherwise default is is,VL = FALSE
#' @param seasonaility if quarterly choose seasonaility = "Quarter" otherwise dont change default is monthly
#' @param xreg Optionally, a numerical vector or matrix of external regressors, which must have the same number of rows as y. (It should not be a data frame.)
#' @param OutOfSample indicates if your are testing out of sample or not, default is false
#'
#' @return a list of data frames with all the forecasts in each frame for each respective column
#' @export
#'
#' @examples # Function call for time series catch all with output DF is a list of forecasted fata frames
#' DF <- time_series_catch <- function(data,  f, h, trainStart, trainEnd, test, n, is.XREG = FALSE, seasonaility = "Monthly") in which data is the orignal data with the variable Date in the first column
#' f is frequency, h is forecasted steps up to 3, trainStart/trainEnd/test are all date vectors specified with c(2016, 1), n is the number of cores to use for pp
#' cols is a vector of equal length of the data column minus 1 which holds the names of all the unique column names.

time_series_catch <- function(data,  f, h, trainStart, trainEnd, test, n, is.XREG = FALSE, seasonaility = "Monthly", xreg = NULL, OutOfSample = FALSE){
  DF=list() #making a list of Data frames
  cols <- colnames(data)
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
    fcastprophet <- ts_prophet(data, h, i, OutOfSample)


    ## formatting time series for other packages
    #setting up the main time series list that will be used in the rest of the models
    time <-ts(ts_data[[i]], frequency = f, start = trainStart)
    n_train<-window(time , start = c(year, month), end = trainEnd) #training

    #dlm model
    a <- ts_dlm_model(time, n_train, h, seasonaility)
    a1 <- as.numeric(n_train)
    a <- c(a1, a)

    #exponntial smoothing
    ETSe <- ets(as.double(n_train))
    ETSf <- fitted.values(ETSe)
    ETS <- forecast(ETSe, h=h)

    if (is.XREG==TRUE){

      #setting up xregs for ARIMA and Hybridmodel
      xregtrain <- xreg[c(1:(NROW(n_train))),1]
      xregtest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),1]
      xregnntrain <- xreg[c(1:(NROW(n_train))),]
      xregnntest <- xreg[c((NROW(n_train)+1):(NROW(n_train)+h)),]

      ## if you go to 1 forecasted value it messes up the format of multiple external regressors
      if (h==1){
        xregnntest <- t(as.matrix(xregnntest))
      }

    }else{}


    if(is.XREG==TRUE){

      #arima modelling
      ARIMAa <- auto.arima(as.double(n_train),
                           stepwise = FALSE, parallel = TRUE, xreg = xregtrain, biasadj = TRUE, ic ="aicc")
      ARIMAf <- fitted.values(ARIMAa)
      ARIMA <- forecast(ARIMA,
                        h=h, xreg = xregtest)
    }else{
      #arima modelling
      ARIMAa <- auto.arima(as.double(n_train),
                           stepwise = FALSE, parallel = TRUE, biasadj = TRUE, ic ="aicc")
      ARIMAf <- fitted.values(ARIMAa)
      ARIMA <- forecast(ARIMAa,
                        h=h)
    }


    #uses tbats model
    TBATSt <- tbats(as.double(n_train),
                    lambda=0, use.parallel = TRUE, num.cores = n)
    TBATSf <- fitted.values(TBATSt)
    TBATS <- forecast(TBATSt,
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
    if (is.XREG==TRUE){

      Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,
                              n.args = list(xreg = xregtrain),
                              a.args = list(xreg = xregtrain))
      Hybrid1f <- fitted.values(Hybrid1h)
      Hybrid1 <- forecast(Hybrid1h,
                          h=h, xreg = xregtest) # ?forecasthybrid

      Hybrid2h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,
                              n.args = list(xreg = xregtrain),
                              a.args = list(xreg = xregtrain))
      Hybrid2f <- fitted.values(Hybrid2h)
      Hybrid2 <- forecast(Hybrid2h,
                          h=h, xreg = xregtest) # ?forecasthybrid

      Hybrid3h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,
                              n.args = list(xreg = xregnntrain))
      Hybrid3f <- fitted.values(Hybrid3h)
      Hybrid3 <- forecast(Hybrid3h,
                          h=h, xreg = xregnntest)

      Hybrid4h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,
                              n.args = list(xreg = xregnntrain))
      Hybrid4f <- fitted.values(Hybrid4h)
      Hybrid4 <- forecast(Hybrid4h,
                          h=h, xreg = xregnntest)





    }else{
      Hybrid1h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,)
      Hybrid1f <- fitted.values(Hybrid1h)
      Hybrid1 <- forecast(Hybrid1h,
                          h=h) # ?forecasthybrid
      Hybrid2h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,)
      Hybrid2f <- fitted.values(Hybrid2h)
      Hybrid2 <- forecast(Hybrid2h,
                          h=h) # ?forecasthybrid
      Hybrid3h <- hybridModel(n_train, models = "aefnt", weights="equal", parallel = TRUE, num.cores = n,)
      Hybrid3f <- fitted.values(Hybrid3h)
      Hybrid3 <- forecast(Hybrid3h,
                          h=h) # ?forecasthybrid
      Hybrid4h <- hybridModel(n_train, models = "aefnt", weights="insample", parallel = TRUE, num.cores = n,)
      Hybrid4f <- fitted.values(Hybrid4h)
      Hybrid4 <- forecast(Hybrid4h,
                          h=h) # ?forecasthybrid
    }


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
                Prophet=tail(fcastprophet, h),
                DLM=tail(a, h))





    # using ?opera package to ensamble the models together and pick the best one best off square loss loss type and model are changeable
    # MLpol0 <- mixture(model = "MLpol", loss.type = "square")
    # Z <- ts(predict(MLpol0, X, as.numeric(n_test),
    #       type='response'),
    #    start=test, freq=f)

    Xf <- cbind.data.frame(ETSf, ARIMAf, TBATSf, Hybrid1f, Hybrid2f, Hybrid3f, Hybrid4f)
    X1 <- data.frame(X)
    X1 <- X1[c(1:7)]
    X1[] <- lapply(X1, as.numeric)
    Xf[] <- lapply(Xf, as.numeric)

    colnames(Xf) <- c("ETS", "ARIMA", "TBATS", "Hybrid1", "Hybrid2", "Hybrid3", "Hybrid4")
    X2 <- rbind(Xf, X1)
    Q <- cbind(X2, fcastprophet, a)

    k <- which(is.na(Q), arr.ind=TRUE)
    Q[k] <- rowMeans(Q, na.rm=TRUE)[k[,1]]
    Q <- head(Q, -h)

    Z2 <- foreccomb(n_train, Q, newpreds = X)
    Z1<- comb_InvW(Z2)
    Z <- c(Z1$Fitted, Z1$Forecasts_Test)

    #combining original and forecast values together
    if (OutOfSample == TRUE){
        Y <- cbind(c(as.numeric(time), rep(NA, h)), X2, fcastprophet, a,  Z)
    } else {
      Y <- cbind(as.numeric(time), X2, fcastprophet, a,  Z)
    }


    #rename columns for completness
    colnames(Y) <- c(
      cols[i],
      "ETS",
      "ARIMAX",
      "TBATS",
      "HybridE",
      "HybridIn",
      "HybridENN",
      "HybridInNN",
      "Prophet",
      "DLM",
      "Ensamble InvW")

    #appending them to an iterated list
    DF[[i]]<-Y
    print(i)  #making sure its loop correctly
  }
  return(DF)
}
