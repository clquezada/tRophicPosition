#' Simulate trophic discrimination factors
#'
#' This function returns trophic discrimination factors (TDF), given a
#' number of observations, a mean and/or a standard deviation for deltaN and/or
#' deltaC.
#'
#' @param nN number of observations for deltaN.
#' @param meanN mean for deltaN.
#' @param sdN standard deviation for deltaN.
#' @param nC number of observations for deltaC.
#' @param meanC mean for deltaC.
#' @param sdC standard deviation for deltaC.
#' @param seed numerical value to indicate reproducible results.
#'
#' @return a named list with TDF values for nitrogen and/or carbon
#' @export
#'
#' @examples
#' # 25 values of TDF for nitrogen, mean 3, sd, 1
#' simulateTDF(nN = 25, meanN = 3, sdN = 1)
#'
#' # 18 values of TDF for carbon, mean 0.6, sd, 0.7
#' simulateTDF(nC = 18, meanC = 0.6, sdC = 0.7)
#'
#'
simulateTDF <- function (nN = 56,
                         meanN = NULL,
                         sdN = 0.98,
                         nC = 107,
                         meanC = NULL,
                         sdC = 1.3,
                         seed = 3) {

  meanSD <- function(x, mean, sd) {

    x <- stats::rnorm(x, mean, sd)
    X <- x
    MEAN <- mean
    SD <- sd
    Z <- (((X - mean(X, na.rm = TRUE))/sd(X, na.rm = TRUE))) * SD
    MEAN + Z
  }
  set.seed(seed = seed)

  deltaN <- NULL
  deltaC <- NULL

  if (!is.null(meanN)) deltaN <- meanSD(nN, meanN, sdN)

  if (!is.null(meanC)) deltaC <- meanSD(nC, meanC, sdC)

  if (!is.null(deltaN) & !is.null(deltaC))
    return (list("deltaC" = deltaC, "deltaN" = deltaN))

  else {
    if (!is.null(deltaN))
      return(deltaN)

    else if (!is.null(deltaC))
      return(deltaC)
  }

  return(list("deltaN" = meanSD(nN, 3.4, sdN), "deltaC" = meanSD(nC, 0.39,
                                                                 sdN)))

}
