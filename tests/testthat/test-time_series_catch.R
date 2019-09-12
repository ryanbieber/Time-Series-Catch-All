test_that("The functions are all running correctly", {


          Date <- seq(as.Date("1974-1-01"), as.Date("1979-12-01"), by="months")
          df <- data.frame(ldeaths, mdeaths, fdeaths)
          data <- cbind(Date, df)

          time_series_catch(data, 12, 3, c(1974, 1), c(1979,9), c(1979, 10), 12)




          })
