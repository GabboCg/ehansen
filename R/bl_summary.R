#' Black-Litterman: Resumen
#'
#' @param mkt_cap Capitalización de mercado
#' @param new_weights Nuevos pesos
#'
#' @return
#' @export
#'
#' @import magrittr
#'
#' @examples
bl_summary <- function (mkt_cap, new_weights) {

  bl_summary_mat <- cbind(mkt_cap * 100,
                          new_weights * 100,
                          (mkt_cap - new_weights) * 100) %>%
    round(2) %>%
    `colnames<-`(c("mktcap", "weights", "diff"))

  return(bl_summary_mat)

}
