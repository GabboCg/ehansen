#' Black Litterman: Portfolio
#'
#' @param bl_er Retorno esperado por BL
#' @param bl_w Pesos por BL
#' @param bl_cov Matriz varianza covarianza por BL
#'
#' @return
#' @export
#'
#' @examples
bl_portfolio <- function(bl_er, bl_w, bl_cov) {

  bl_er_p <- t(bl_er) %*% bl_w
  bl_sd_p <- sqrt(t(bl_w) %*% bl_cov %*% bl_w)

  bl_p <- list(er = as.numeric(bl_er_p), sd = as.numeric(bl_sd_p))

  return(bl_p)

}
