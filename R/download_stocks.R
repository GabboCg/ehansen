#' Descarga acciones
#'
#' @import magrittr dplyr stringr tibble
#' @importFrom quantmod getSymbols
#' @importFrom rlang .data
#'
#' @param stocks Vectores con el nombre de los tickers
#' @param periodicity Frecuencia
#' @param from Desde
#' @param to Hasta
#'
#' @return
#' @export
#'
#' @examples
download_stocks <- function (stocks, periodicity, from, to) {

  quantmod::getSymbols(stocks,
                       periodicity = periodicity,
                       from = from,
                       to = to,
                       src = "yahoo",
                       auto.assign = TRUE)

  list_stocks <- list()

  for (i in seq_along(stocks)) {

    df <- get(stocks[i]) %>%
      tibble::as_tibble(rownames = "rowname") %>%
      rename(date = .data$rowname) %>%
      mutate(symbol = stocks[i]) %>%
      rename_at(vars(starts_with(paste0(stocks[i], "."))),
                     list(~ tolower(stringr::str_replace(.,
                                                         paste0(stocks[i],
                                                                "."), ""))))
    list_stocks[[i]] <- df

  }

  stocks_tbl <- do.call(rbind, list_stocks)

  return(stocks_tbl)

}
