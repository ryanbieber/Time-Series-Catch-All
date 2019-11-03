#' getting error values from the best model
#'
#' @param data original data
#' @param DFError list of data frames that contain errors
#' @param x indicator for mape(default)=1, x=2 rmse, x=3 mse
#'
#' @return numeric vector of best error values corresponding to n-1 column
#' @export
#'
#' @examples q <- extract_best_error(data, DFError)
extract_best_error <- function(data, DFError, x=1){
  best_error = list()
  for (i in 2:length(data)){

    y <- min(DFError[[i]][x,])
    best_error[[i]] <- y

    error <- unlist(best_error)

  }

  return(error)
}

