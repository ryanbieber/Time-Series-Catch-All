##
#' dlm model
#'
#' @param time ts
#' @param n_train ts
#' @param h integer from 1-3
#' @param seasonaility monthly by default
#'
#' @return forecasted values based on h
#' @export
#'
#' @examples gets called in the time_series_catch_all function to run DLM models.
ts_dlm_model <- function(time, n_train, h, seasonaility){


  ##DLM use ?dlm in command prompt
  if (seasonaility=="Monthly"){
    model.mle <- dlmMLE(time, parm=c(0.1, 0, 1, 1), build=model.build.12)
    model.fit <- model.build.12(model.mle$par)
  } else{
    model.mle <- dlmMLE(time, parm=c(0.1, 0, 1, 1), build=model.build.3)
    model.fit <- model.build.3(model.mle$par)
  }

  model.filtered <- dlmFilter(time, model.fit)
  model.smoothed <- dlmSmooth(time, model.fit)
  model.forecast <- dlmForecast(model.filtered, nAhead=h)
  xf <- seq(max(index(n_train)), max(index(n_train))+h/12, 1/12)
  xf <- xf[(-1)]
  aa <- model.forecast$a[,-1]*(-1)
  aa <- cbind(model.forecast$a[,1], aa)
  a <- drop(model.forecast$a%*%t(FF(model.fit)))
  return(a)

  # interesting data frame
  # df <- rbind(
  #   data.frame(x=index(n_train), y=as.numeric(n_train), series="original"),
  #   data.frame(x=index(n_train), y=head(apply(model.filtered$m[-1,1:2], 1, sum), -3), series="filtered"),
  #   data.frame(x=index(n_train), y=head(apply(model.smoothed$s[-1,1:2], 1, sum), -3), series="smoothed"),
  #   data.frame(x=xf, y=a, series="forecast")
  # )

}
