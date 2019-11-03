test_that("The functions are all running correctly", {

  # building the test data
         test <- replicate(5, list(ldeaths))
         expect_error(test_model <- par_time_series_catch(test, num.cores = 12))
         test_forecast <- extract_model_fit_forecast(test_model, num.cores = 12)
          })


