#' Simulate trophic enrichment factors
#'
#' \code{simulateTEF} return trophic enrichment factors (TEF), given a number of
#' observations, a mean and a standard deviation.
#'
#'
#' @param nN number of observations for deltaN
#' @param meanN mean for deltaN
#' @param sdN standard deviation for deltaN
#' @param nC number of observations for deltaC
#' @param meanC mean for deltaC
#' @param sdC standard deviation for deltaC
#'
#' @return
#' @export
#'
#' @examples
#'
simulateTEF <- function (nN = 56,
                         meanN = 3.4,
                         sdN = 0.98,
                         nC = 107,
                         meanC = 0.39,
                         sdC = 1.3) {

  meanSD <- function(x, mean, sd) {

    x <- stats::rnorm(x, mean, sd)
    X <- x
    MEAN <- mean
    SD <- sd
    Z <- (((X - mean(X, na.rm = TRUE))/sd(X, na.rm = TRUE))) * SD
    MEAN + Z
  }

  deltaC = meanSD(nN, meanC, sdC)
  deltaN = meanSD(nC, meanN, sdN)

  return (list("deltaC" = deltaC, "deltaN" = deltaN))

}
