#' Black-Litterman: Retorno de equilibrio
#'
#' @param ra something
#' @param cov something
#' @param mktcapw something
#' @param rf something
#'
#' @return
#' @export
#'
#' @examples
bl_eqret <- function (ra, cov, mktcapw, rf) {

  eqret <- ra * cov %*% as.vector(mktcapw) + rf

  return(eqret)

}
