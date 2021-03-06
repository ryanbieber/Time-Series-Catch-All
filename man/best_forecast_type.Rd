% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/par_catch_all.R
\name{best_forecast_type}
\alias{best_forecast_type}
\title{Picks the best model based on mape and smape and outputs a list}
\usage{
best_forecast_type(model_forecast, steps = steps, origin = NULL)
}
\arguments{
\item{model_forecast}{output from extract_model_fit_forecast}

\item{steps}{numeric based on how far you want to look back and ahead}

\item{origin}{series you are comparing agianst}
}
\value{
list of data frames with mape and smape models
}
\description{
Picks the best model based on mape and smape and outputs a list
}
\examples{
\dontrun{
test <- replicate(5, list(ldeaths))
test_model <- par_time_series_catch(test, num.cores = 12)
}

}
