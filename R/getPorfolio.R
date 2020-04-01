#' Portafolio
#'
#' @param er Retornos esperados
#' @param cov.mat Matriz de varianza-covarianza
#' @param weights Ponderaciones
#'
#' @return
#' @export
#'
#' @examples
getPortfolio <- function(er, cov.mat, weights){

  call <- match.call()

  #
  # check for valid inputs
  #
  asset.names <- names(er)
  weights <- as.vector(weights)
  names(weights) = names(er)
  er <- as.vector(er) # assign names if none exist
  if(length(er) != length(weights))
    stop("dimensions of er and weights do not match")
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("dimensions of er and cov.mat do not match")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")

  #
  # create portfolio
  #
  er.port <- crossprod(er,weights)
  sd.port <- sqrt(weights %*% cov.mat %*% weights)
  ans <- list("call" = call,
              "er" = as.vector(er.port),
              "sd" = as.vector(sd.port),
              "weights" = weights)
  class(ans) <- "portfolio"
  return(ans)
}
