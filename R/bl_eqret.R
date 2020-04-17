#' Black-Litterman: Retorno de equilibrio
#'
#' @param ra Aversion al riesgo
#' @param cov Matriz de varianza y covarianza
#' @param mktcapw Capitalización de mercado
#' @param rf Tasa libre de riesgo
#'
#' @return
#' @export
#'
#' @examples
bl_eqret <- function (ra, cov, mktcapw, rf) {

  eqret <- ra * cov %*% as.vector(mktcapw) + rf

  return(eqret)

}
