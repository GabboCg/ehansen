#' Black-Litterman: Retorno esperado
#'
#' @param mu something
#' @param cov something
#' @param pmat something
#' @param qmat something
#' @param tau something
#' @param omega something
#' @param confidence something
#'
#' @return
#' @export
#'
#' @examples

bl_posterior <- function (mu, cov, pmat = NULL, qmat, tau, omega = NULL, confidence = NULL) {

  out <- list()

  if (is.null(omega)) {

    if (!is.null(pmat)) {

      if (is.null(confidence)) {

        omega <- diag(c(1, diag(tau * pmat %*% cov %*% t(pmat))))[-1, -1]

      } else {

        omega <- diag(c(1, confidence))[-1, -1]

      }

      temp <- solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)

      out$cov <- cov + temp
      out$expected_return <- temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat)

    } else {

      temp <- solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)

      out$cov <- cov + temp
      out$expected_return <- temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat)

    }

  } else {

    temp <- solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)

    out$cov <- cov + temp
    out$expected_return <- temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat)

  }

  return(out)

}
