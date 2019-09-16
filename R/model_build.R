#' Model building for DLM
#'
#' @param p Used in DLM modelling only
#'
#' @return DLM
#' @export
#'
#' @examples Monthly seasonal model
model.build.12 <- function(p) {
  return(
    dlmModPoly(2, dV=p[1], dW=p[2:3]) +
      dlmModSeas(12, dV=p[4])
  )
}


