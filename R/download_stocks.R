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

    get_stocks <- ifelse(stringr::str_detect(stocks[i], "^"),
                         stringr::str_replace(stocks[i], "[\\^]", ""),
                         stocks[i])

    df <- get(get_stocks) %>%
      tibble::as_tibble(rownames = "rowname") %>%
      rename(date = .data$rowname) %>%
      mutate(symbol = get_stocks) %>%
      rename_at(vars(starts_with(paste0(get_stocks, "."))),
                list(~ tolower(stringr::str_replace(.,
                                                    paste0(get_stocks, "."), ""))))
    list_stocks[[i]] <- df

  }

  stocks_tbl <- do.call(rbind, list_stocks)

  return(stocks_tbl)

}
