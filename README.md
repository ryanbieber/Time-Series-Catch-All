# Looks at time series


## How to use the package on normal time series data

After the package is installed using ```devtools::install_github("ryanbieber/Time-Series-Catch-All")``` , The following code sections go over what the package does.

Using some base data about deaths of lung disease for women we make a list of time series.

``` r
listTS <- replicate(2, list(ldeaths))
```

After this we can use the function, ``` par_time_series_catch()``` to model each series with different types of models.

```r
## num.cores indicates the amount of cores you want to use in this process
models <- par_time_series_catch(listTS, num.cores = 12)
```

After running this we can now use the ```extract_model_fit_forecast()``` with our list of models we have indicated.

```r
model_forecasts <- extract_model_fit_forecast(models, num.cores = 12)
```

This will give you a list of numeric vectors that contain the fitted values and forecasts. One small note to make is that for some reason that I havent figured out yet ```hybridModel()``` doesn't work with ```parLapply()``` therefore the forecasting is slower than it could be.
