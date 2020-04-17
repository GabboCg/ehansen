#' Black-Litterman: Risk Aversion
#'
#' @param bench Activo benchmark
#' @param rf Tasa libre de riesgo
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
