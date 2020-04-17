#' Importar matriz varianza-covarianza
#'
#' @param cov_mat something
#'
#' @return
#' @export
#'
#' @import readr
#'
#' @examples
import_cov_mat <- function (cov_mat) {

  cov_mat_tbl <- readr::read_csv2(cov_mat) %>%
    as.matrix()

  return(cov_mat_tbl)

}
