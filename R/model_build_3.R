#' Model building for DLM
#'
#' @param p used in DLM modelling
#'
#' @return DLM
#' @export
#'
#' @examples
model.build.3 <- function(p) {
  return(
    dlmModPoly(2, dV=p[1], dW=p[2:3]) +
      dlmModSeas(4, dV=p[4])
  )
}
