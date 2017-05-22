#' Summary for stable isotope data
#'
#' @param isotopeData an isotopeData class object.
#' @param print a logical value to indicate whether the summary is printed.
#' @param round_dec number of decimals keeped.
#' @param ... additional arguments passed to this function.
#'
#' @return a list with number of observations, mean, standard deviation,
#' standard error, minimum, maximum and median for each element of an
#' isotopeData class object.
#' @export
#'
#' @examples
#' ## S3 method for class 'isotopeData'
#' a <- generateTPData()
#' summary(a)
#'
summary <- function (isotopeData, print = TRUE, round_dec = 1, ...) {
  UseMethod("summary")
}

#' Summary for stable isotope data
#'
#' @param isotopeData an isotopeData class object.
#' @param print a logical value to indicate whether the summary is printed.
#' @param round_dec number of decimals keeped.
#' @param ... additional arguments passed to this function.
#'
#' @return a list with number of observations, mean, standard deviation,
#' standard error, minimum, maximum and median for each element of an
#' isotopeData class object.
#' @export
#'
#' @examples
#' a <- generateTPData()
#' summary(a)
#'
summary.isotopeData <- function (isotopeData, print = TRUE, round_dec = 1, ...) {

  if (class(isotopeData) != "isotopeData") stop("The object don't have a class isotopeData")

  #class(isotopeData) <- "list"

  #To Do: do.call(rbind, lapply(my.list, data.frame, stringsAsFactors=FALSE))
  #
  sef <- function(x) sd(x)/sqrt(length(x))

  n <- lapply(isotopeData, length)
  min <- lapply(lapply(isotopeData, min), round, round_dec)
  mean <- lapply(lapply(isotopeData, mean), round, round_dec)
  median <- lapply(lapply(isotopeData, median), round, round_dec)
  max <- lapply(lapply(isotopeData, max), round, round_dec)
  sd <- lapply(lapply(isotopeData, sd), round, round_dec)
  #sqrt <- lapply(lapply(n, sqrt), round, round_dec)
  se  <- lapply(lapply(isotopeData,sef), round, round_dec)
  #se  <- lapply(as.list(mapply("/",sqrt,sd)), round, round_dec)

  if (isTRUE(print)) {

    cat("\nSummary for stable isotope data object", "\n\n")

    if (!is.null(attributes(isotopeData)$consumer))
      cat("Community ", attributes(isotopeData)$community, "\n")
    if (!is.null(attributes(isotopeData)$consumer))
      cat("Consumer ", attributes(isotopeData)$consumer, "\n")
    if (!is.null(attributes(isotopeData)$baseline1))
      cat("Baseline 1 ", attributes(isotopeData)$baseline1, "\n")
    if (!is.null(attributes(isotopeData)$baseline2))
      cat("Baseline 2 ", attributes(isotopeData)$baseline2, "\n")
    cat("\n")
  }

  cbind(n,mean,sd,se,min,max,median)

}
