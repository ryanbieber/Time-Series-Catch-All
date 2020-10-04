# Looks at time series


## How to use the package on normal time series data

After the package is installed using ```devtools::install_github("ryanbieber/Time-Series-Catch-All")``` , The following code sections go over what the package does.


``` r
library(TimeSeriesCatchAll)
```
All you have to do now is make sure that your series you want to test are in a data-frame with the column names being the names of the series you want to test. That's it, it will also impute missing values and look for anomalies as well. If it does change a value it will tell you in what column and in what position the value was that got changed.

```r
## num.cores indicates the amount of cores you want to use in this process
models <- all_in_one_time_series(mtcars, num.cores = parallel::detectCores()-1)
```

Disclaimer: I know mtcars isn't a time-series but it will work nonetheless.

The output will be a data-frame with your model used on the bottom of however many steps you decided to look out with the column name being the name of the original series. Enjoy! This should hopefully save you a lot of time deciding on what model to choose for your time-series data.

#### Main Packages used
library(forecast) [Documentation](https://cran.r-project.org/web/packages/forecast/forecast.pdf)
library(forecastHybrid) [Documentation](https://cran.r-project.org/web/packages/forecastHybrid/forecastHybrid.pdf)
library(dlm) [Documentation](https://cran.r-project.org/web/packages/dlm/dlm.pdf)
library(mice) [Documentation](https://cran.r-project.org/web/packages/mice/mice.pdf)
library(outForest) [Documentation](https://cran.r-project.org/web/packages/outForest/vignettes/outRanger.html)

