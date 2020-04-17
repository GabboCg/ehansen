#' Importar visiones de los inversionistas
#'
#' @param investor_views something
#'
#' @return
#' @export
#'
#' @import readr
#'
#' @examples
import_investor_views <- function (investor_views) {

  investor_views_tbl <- readr::read_csv2(investor_views) %>%
  # select(sort(current_vars())) %>%
  as.matrix()

  return(investor_views_tbl)

}

