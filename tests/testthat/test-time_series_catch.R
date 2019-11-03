test_that("The functions are all running correctly", {
        library(TimeSeriesCatchAll)
         test <- replicate(2, list(ldeaths))
         test_model <- par_time_series_catch(test)
         test_forecast <- extract_model_fit_forecast(test_model)
          })


