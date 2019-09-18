test_that("The functions are all running correctly", {

  # building the test data
          Date <- seq(as.Date("1974-1-01"), as.Date("1979-12-01"), by="months")
          df <- data.frame(ldeaths, mdeaths, fdeaths)
          data <- cbind(Date, df)
          f=12
          h=6
          trainStart = c(1974,1)
          trainEnd = c(1979,6)
          testStart = c(1979, 7)
          n=12
  # main testing function
          expect_warning( DF <- time_series_catch(data, f, h, trainStart, trainEnd, testStart, n))
  # errors
          DFError <- error_values(DF, h)
  # best forecast by error
          DFMAPE <- best_forecast_by_MAPE(DF, data,  f, DFError, trainStart)
          DFMSE <- best_forecast_by_MSE(DF, data,  f, DFError, trainStart)
          DFRMSE <- best_forecast_by_RMSE(DF, data,  f, DFError, trainStart)
  # name of the best model
          MAPE <- name_of_best_model(data, DFError, x=1)
          RMSE <- name_of_best_model(data, DFError, x=2)
          MSE <- name_of_best_model(data, DFError, x=3)
  # output to csv
          csv_forecast_values(data, DFMAPE, h, MAPE, DFError, x=1)
          csv_forecast_values(data, DFRMSE, h, RMSE, DFError, x=2)
          csv_forecast_values(data, DFMSE, h, MSE, DFError, x=3)
  # fast catch_all
         expect_warning( DFFast <- fast_time_series_catch_all(data, f, h, trainStart, trainEnd, testStart, n, MAPE))

          })
