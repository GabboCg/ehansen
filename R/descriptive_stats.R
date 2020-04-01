#' Estadistica Descriptiva
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom stats sd
#' @importfrom tidyr spread
#' @importFrom PerformanceAnalytics skewness
#' @importFrom PerformanceAnalytics kurtosis
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
                                 PerformanceAnalytics::skewness(x, na.rm = TRUE),
                                 PerformanceAnalytics::kurtosis(x, na.rm = TRUE, method = "moment"))) %>%
    bind_rows() %>%
    t() %>%
    `colnames<-`(c('Mean', 'Std', "Skew", "Kurt")) %>%
    as_tibble(rownames = "rowname") %>%
    rename(Stocks = .data$rowname) %>%
    select(.data$Stocks, .data$Mean, .data$Std, .data$Skew, .data$Kurt) %>%
    mutate_at(vars(.data$Mean:.data$Kurt), round_fun)

  return(describe_stats)

}
