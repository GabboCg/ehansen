#' Retorno Mensual
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @param stocks_tbl Activos
#' @param type Tipo de retorno, log = "logaritmo" y arithmetic = aritmetico
#'
#' @return
#' @export
#'
#' @examples
monthly_return <- function (stocks_tbl, type = c("log", "arithmetic")) {

  return_tbl <- stocks_tbl %>%
    select(.data$date, .data$close, .data$symbol) %>%
    group_by(.data$symbol) %>%
    mutate(return = switch(type,
                           "log" = log(close / dplyr::lag(close)),
                           "arithmetic" = (close - lag(close) / lag(close)))) %>%
    filter(!is.na(return)) %>%
    select(date, return, .data$symbol)

    return(return_tbl)

}
