test_that("The functions are all running correctly", {

  # building the test data
          Date <- seq(as.Date("1974-1-01"), as.Date("1979-12-01"), by="months")
          df <- data.frame(ldeaths, mdeaths, fdeaths)
          data <- cbind(Date, df)
  # main testing function
          expect_warning( DF <- time_series_catch(data, 12, 3, c(1974, 1), c(1979,9), c(1979, 10), 12))
  # errors
          DFError <- error_values(DF, 3)
  # best forecast by error
          DFMAPE <- best_forecast_by_MAPE(DF, data,  12, DFError, c(1974, 1))
          DFMSE <- best_forecast_by_MSE(DF, data,  12, DFError, c(1974, 1))
          DFRMSE <- best_forecast_by_RMSE(DF, data,  12, DFError, c(1974, 1))
  # name of the best model
          MAPE <- name_of_best_model(data, DFError, x=1)
          RMSE <- name_of_best_model(data, DFError, x=2)
          MSE <- name_of_best_model(data, DFError, x=3)
  # output to csv
          csv_forecast_values(data, DFMAPE, 3, MAPE, DFError, x=1)
          csv_forecast_values(data, DFMAPE, 3, RMSE, DFError, x=2)
          csv_forecast_values(data, DFMAPE, 3, MSE, DFError, x=3)
  # fast catch_all
         expect_warning( DFFast <- fast_time_series_catch_all(data, 12, 3, c(1974, 1), c(1979,9), c(1979, 10), 12, MAPE))

          })
