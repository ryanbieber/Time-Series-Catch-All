##

#' name of best model (4)
#'
#' @param data original data with Date in first column
#' @param modellist list of data frames that contain errors
#' @param x integer indicating MAPE, MSE, or RMSE
#'
#' @return List of he best model names used
#' @export
#'
#' @examples Takes the original data used and the list of errors made from error_calculatons to find out which is the best model.
#' mape <- name_of_best_model(data, DFError)
name_of_best_model <- function(data, modellist, x=1){
  namesList=list()
  for (i in 2:length(data)){
    namesError <- names(as.data.frame(modellist[[i]]))
    namesError <- namesError[match(min(modellist[[i]][x,]), modellist[[i]][x,])]
    namesList[[i]] <- namesError
  }
  bestModels <- as.character(unlist(namesList))
  return(bestModels)
}
