# Looks at time series in IBM finance

## Installation
First, install `rtools` from [CRAN](https://cran.r-project.org/bin/windows/Rtools/)

Now, download the most recent package from [BOX](https://ibm.box.com/s/0t2rjejy9p7inaq6drlam2y7lmfxhioy)

Then, from inside `R` run:

``` r
install_local("NameOfTheMostUpToDateFile.tar.gz", repos = NULL, type="source", dependencies = TRUE)
```

## How to use the package on normal time series data

After the package is installed the main functions you will use are,

``` r
time_series_catch, 
error_calculations, 
best_forecast_by_MAPE/MSE/RMSE, 
best_model_name, 
fast_time_series_catch_all,
and csv_forecast_values
```

First of all, you are going to want to get your data into the correct format and that is in the first column has to be the Date value of your variables.
with the rest of the columns being numeric with unique names attached to each one. Let's start with an example data set from base R which contains deaths from lung disease by month for males, females, and both.


``` r
Date <- seq(as.Date("1974-1-01"), as.Date("1979-12-01"), by="months")
df <- data.frame(ldeaths, mdeaths, fdeaths)
data <- cbind(Date, df)
```

Now we need to call the package,


``` r
library(TimeSeriesCatchAll)
```
After we do that we are able to start calling the functions that will do the work for us,

``` r
DF <- time_series_catch(data, 12, 3, c(1974, 1), c(1979,9), c(1979, 10), 12)
```

Now that we have our list of values called DF we need to run our error calculations on it,


``` r
DFError <- error_values(DF, 3)
```

After this we have the errors all calculated for each individual forecast by each column in our data frame. Now, let us say we want to only look at MAPE and pick the best forecast by that error metric. We are then going to use the next function,

``` r
DFMAPE <- best_forecast_by_MAPE(DF, data,  12, DFError, c(1974, 1))
```

Now that we have our best models by MAPE, lets get the model names out as well, x=1 is on by default this step is only needed if you plan on running the model again but only using the models that were the best preforming. Which is what our `fast_time_series_catch_all` is for.

``` r
# equal the same thing
MAPE <- name_of_best_model(data, DFError)
MAPE <- name_of_best_model(data, DFError, x=1)


```

After we do this step we are almost done. All we need to do now is output the values and the model type to a csv which is done by,

``` r
csv_forecast_values(data, DFMAPE, 3, MAPE)
```

This will output a file in your working directory call forecast_values.csv. I hope you enjoyed using this package as much I did when making it.

#### **Only run this next step if you have already ran the time_series_catch at least once**


```r
DFFast <- fast_time_series_catch_all(data, 12, 3, c(1974, 1), c(1979,9), c(1979, 10), 12, MAPE)
```

