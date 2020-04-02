#' Estadistica Descriptiva
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom stats sd
#' @importfrom tidyr spread
#' @importFrom PerformanceAnalytics skewness
#' @importFrom PerformanceAnalytics kurtosis
#' @importFrom tseries jarque.bera.test
#' @importFrom stats acf
#'
#' @param return_tbl Retornos
#'
#' @return
#' @export
#'
#' @examples
descriptive_stats <- function(return_tbl) {

  round_fun <- function(x) round(x, 5)

  describe_stats <- return_tbl %>%
    select(.data$date, .data$return, .data$symbol) %>%
    group_by(.data$symbol) %>%
    tidyr::spread(.data$symbol, .data$return) %>%
    select(-.data$date) %>%
    purrr::map(function(x) cbind(mean(x, na.rm = TRUE),
                                 sd(x, na.rm = TRUE),
                                 min(x, na.rm = TRUE),
                                 max(x, na.rm = TRUE),
                                 PerformanceAnalytics::skewness(x, na.rm = TRUE),
                                 PerformanceAnalytics::kurtosis(x, na.rm = TRUE,
                                                                method = "moment"),
                                 tseries::jarque.bera.test(x)$p.value,
                                 acf(x, plot = FALSE)[[1]][2])) %>%
    bind_rows() %>%
    t() %>%
    `colnames<-`(c('Mean', 'Std', "Min", "Max", "Skew", "Kurt", "JB", "AC(1)")) %>%
    as_tibble(rownames = "rowname") %>%
    rename(Stocks = .data$rowname) %>%
    select(.data$Stocks, .data$Mean, .data$Min, .data$Max, .data$Std,
           .data$Skew, .data$Kurt, .data$JB, .data$`AC(1)`) %>%
    mutate_at(vars(.data$Mean:.data$`AC(1)`), round_fun)

  return(describe_stats)

}
