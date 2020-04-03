#' Black-Litterman: Risk Aversion
#'
#' @param bench something
#' @param rf risk free
#'
#' @importFrom stats var
#'
#' @return
#' @export
#'
#' @examples
bl_risk_aversion <- function (bench, rf) {

  lambda <- (mean(bench) - rf) / var(bench)

}
