#' Model building for DLM
#'
#' @param p Used in DLM modelling only
#'
#' @return DLM
#' @export
#'
#' @examples makes a seasonal model in the dlm sense with 2 polynomial terms
model.build.12 <- function(p) {
  return(
    dlmModPoly(2, dV=p[1], dW=p[2:3]) +
      dlmModSeas(12, dV=p[4])
  )
}


