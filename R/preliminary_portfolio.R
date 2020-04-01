#' Preliminares para el portafolio
#'
#' @param returns Dataframe o tibble con los retornos
#'
#' @importFrom tidyr spread
#' @importFrom dplyr select
#' @importFrom stats sd
#' @importFrom stats cov
#' @importFrom rlang .data
#'
#' @return
#' @export
#'
#' @examples
preliminary_portfolio <- function(returns) {

  df <- returns %>%
      tidyr::spread(.data$symbol, .data$return) %>%
      dplyr::select(-date)

  mean_ret <- apply(df, 2, mean)
  sd_ret <- apply(df, 2, sd)
  cov_ret <- cov(df)

  list_output <- list(mean_ret, sd_ret, cov_ret)
  names(list_output) <- c("Mean", "Sd", "VarCov")

  return(list_output)

}


