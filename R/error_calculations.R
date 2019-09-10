#
#' making the error values for all the models and adding them to one giant list (2)
#'
#' @param DF list of data frames
#' @param h integer of forecasted steps
#'
#' @return DFError or a list of error dataframes
#' @export
#'
#' @examples DFError <- error_values(DF, h) uses the list of data frames made by time_series_catch_all and h is the forecasted steps.
error_values <- function(DF, h){
  DFError=list() #making a list of data frames that hold error calcualtions
  for (i in 2:length(data)){
    #Getting the predicted value based off model location, orginal data is in col 1 and forecasted is in the subsequent
    MAPE1<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,2],h)))
    RMSE1<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,2],h)))
    MSE1<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,2],h)))
    MAPE2<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,3],h)))
    RMSE2<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,3],h)))
    MSE2<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,3],h)))
    MAPE3<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,4],h)))
    RMSE3<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,4],h)))
    MSE3<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,4],h)))
    MAPE4<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,5],h)))
    RMSE4<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,5],h)))
    MSE4<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,5],h)))
    MAPE5<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,6],h)))
    RMSE5<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,6],h)))
    MSE5<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,6],h)))
    MAPE6<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,7],h)))
    RMSE6<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,7],h)))
    MSE6<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,7],h)))
    MAPE7<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,8],h)))
    RMSE7<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,8],h)))
    MSE7<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,8],h)))
    MAPE8<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,9],h)))
    RMSE8<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,9],h)))
    MSE8<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,9],h)))
    MAPE9<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,10],h)))
    RMSE9<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,10],h)))
    MSE9<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,10],h)))
    MAPE10<- mape(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,11],h)))
    RMSE10<- rmse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,11],h)))
    MSE10<- mse(as.numeric(tail(DF[[i]][,1],h)),as.numeric(tail(DF[[i]][,11],h)))
    error1 <- rbind(MAPE1, RMSE1, MSE1)
    error2 <- rbind(MAPE2, RMSE2, MSE2)
    error3 <- rbind(MAPE3, RMSE3, MSE3)
    error4 <- rbind(MAPE4, RMSE4, MSE4)
    error5 <- rbind(MAPE5, RMSE5, MSE5)
    error6 <- rbind(MAPE6, RMSE6, MSE6)
    error7 <- rbind(MAPE7, RMSE7, MSE7)
    error8 <- rbind(MAPE8, RMSE8, MSE8)
    error9 <- rbind(MAPE9, RMSE9, MSE9)
    error10 <- rbind(MAPE10, RMSE10, MSE10)
    error <- cbind(error1, error2, error3, error4, error5, error6, error7, error8, error9, error10)
    colnames(error) <- c("ETS", "ARIMAX", "TBATS", "HybridE", "HybridIn", "HybridENN", "HybridInNN",  "Prophet","DLM", "Mixture")
    DFError[[i]] <- error
  }
  return(DFError)
}

