# Looks at time series


## How to use the package on normal time series data

After the package is installed using ```devtools::install_github("ryanbieber/Time-Series-Catch-All")``` , The following code sections go over what the package does.

Using some base data about deaths of lung disease for women we make a list of time series.

``` r
library(TimeSeriesCatchAll)
```

``` r
listTS <- replicate(2, list(ldeaths))
```

Now that you have your time-series in a list all we have to do is run, ``` par_time_series_catch()``` to model each series with different types of models. This will backtest all the models based on the steps and then produces a forecast based on the best one out as many steps as you have looked back.

```r
## num.cores indicates the amount of cores you want to use in this process
models <- par_time_series_catch(listTS, num.cores = 12)
```

The output is a dataframe of your forecast with the model type on top, if you change the error type it may change the model outputs based on mape or smape. It is the simplest forecasting method for time-series out there. A cavet though, you have to have all the time-series start and end in the same time-period. So, if you have multiple different time-series lengths, seperate them into similar sizes and then run the forecasting function.
