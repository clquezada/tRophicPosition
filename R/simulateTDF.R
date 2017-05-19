#' Simulate trophic discrimination factors
#'
#' \code{simulateTDF} return trophic discrimination factors (TDF), given a number of
#' observations, a mean and a standard deviation for DeltaN and/or DeltaC.
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
simulateTDF <- function (nN = 56,
                         meanN = NULL,
                         sdN = 0.98,
                         nC = 107,
                         meanC = NULL,
                         sdC = 1.3) {

  meanSD <- function(x, mean, sd) {

    x <- stats::rnorm(x, mean, sd)
    X <- x
    MEAN <- mean
    SD <- sd
    Z <- (((X - mean(X, na.rm = TRUE))/sd(X, na.rm = TRUE))) * SD
    MEAN + Z
  }

  deltaN = NULL
  deltaC = NULL

  if (!is.null(meanN)) deltaN = meanSD(nN, meanN, sdN)

  if (!is.null(meanC)) deltaC = meanSD(nC, meanC, sdC)

  if (!is.null(deltaN) & !is.null(deltaC))
    return (list("deltaC" = deltaC, "deltaN" = deltaN))

  else {
    if (!is.null(deltaN))
      return(deltaN)

    else if (!is.null(deltaC))
      return(deltaC)
  }

  return(list("deltaN" = meanSD(nN, 3.4, sdN), "deltaC" = meanSD(nC, 0.39, sdN)))

}
