test_that("The functions are all running correctly", {
        library(TimeSeriesCatchAll)
        #library(Rcpp)
         test <- replicate(2, list(ldeaths))
         test_model <- TimeSeriesCatchAll::par_time_series_catch(test)
          })


