% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/par_catch_all.R
\name{dlmPara}
\alias{dlmPara}
\title{DLM building function that models and forecast a dlm model for each TS in the par_catch_all cal}
\usage{
dlmPara(x, dlmPoly = dlmPoly, dlmSeas = dlmSeas, steps = steps, freq = freq)
}
\arguments{
\item{x}{ts}

\item{dlmPoly}{polynomial order used in model.build}

\item{dlmSeas}{seasonal portion}

\item{steps}{steps ahead}

\item{freq}{"month"}
}
\value{
array of fitted + forecast
}
\description{
DLM building function that models and forecast a dlm model for each TS in the par_catch_all cal
}
\examples{
\dontrun{
test <- replicate(5, list(ldeaths))
test_model <- par_time_series_catch(test, num.cores = 12)
}
}
