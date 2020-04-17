#' Importa datos bursatil
#'
#' @param data something
#'
#' @return
#' @export
#'
#' @import dplyr readr
#' @importFrom tidyr gather
#' @importFrom rlang .data
#'
#' @examples
import_data <- function(data) {

  data_tbl <- readr::read_csv2(data) %>%
    rename(date = .data$Date) %>%
    tidyr::gather(key = "symbol", value = "close", -.data$date)

  return(data_tbl)

}
