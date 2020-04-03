#' Black-Litterman: Optimal Weights
#'
#' @param ra something
#' @param mu something
#' @param cov something
#'
#' @return
#' @export
#'
#' @examples
bl_optimal <- function (ra, mu, cov) {

  optimal <- (1 / ra) * solve(cov) %*% mu

  return(optimal)

}
