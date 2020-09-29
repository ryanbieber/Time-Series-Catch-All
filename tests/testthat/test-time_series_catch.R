test_that("The functions are all running correctly", {
        library(TimeSeriesCatchAll)
        test <- cbind.data.frame(ldeaths, mdeaths)
        test_model <- TimeSeriesCatchAll::all_in_one_time_series(test)
          })


