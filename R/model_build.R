#' Model building for DLM
#'
#' @param p Used in DLM modelling only
#'
#' @return DLM
#' @export
#'
#' @examples makes a quarterly seasonal model in the dlm sense with 2 polynomial terms
model.build <- function(p) {
  return(
    dlmModPoly(2, dV=p[1], dW=p[2:3]) +
      dlmModSeas(4, dV=p[4])
  )
}
