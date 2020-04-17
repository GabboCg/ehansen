#' Importa visiones
#'
#' @param import_views something
#'
#' @return
#' @export
#'
#' @import readr
#'
#' @examples
import_views <- function (import_views) {

  import_views <- readr::read_csv2(import_views) %>%
    as.matrix()

  return(import_views)

}
