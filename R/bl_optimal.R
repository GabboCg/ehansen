#' Black-Litterman: Optimal Weights
#'
#' @param ra Aversión al riesgo
#' @param mu Retornos
#' @param cov Matriz varianza y covarianza
#'
#' @return
#' @export
#'
#' @examples
bl_optimal <- function (ra, mu, cov) {

  optimal <- (1 / ra) * solve(cov) %*% mu

  return(optimal)

}
