#' Importa Market Capitalization
#'
#' @param mkt_cap something
#'
#' @return
#' @export
#'
#' @import dplyr readr
#'
#' @examples
import_mkt_cap <- function (mkt_cap) {

  mkt_cap_df <- readr::read_csv2(mkt_cap) %>%
    select(sort(current_vars())) %>%
    t()

  return (mkt_cap_df)

}

