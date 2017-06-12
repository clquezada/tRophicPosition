#' Summary for stable isotope data
#'
#' @param object an isotopeData class object.
#' @param print a logical value to indicate whether the summary is printed.
#' @param round_dec number of decimals kept.
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
summary.isotopeData <- function (object, print = TRUE, round_dec = 1, ...) {

  if (class(object) != "isotopeData") stop("The object don't have a class isotopeData")

  #class(isotopeData) <- "list"

  #To Do: do.call(rbind, lapply(my.list, data.frame, stringsAsFactors=FALSE))
  #
  sef <- function(x) sd(x)/sqrt(length(x))

  n <- lapply(object, length)
  min <- lapply(lapply(object, min), round, round_dec)
  mean <- lapply(lapply(object, mean), round, round_dec)
  median <- lapply(lapply(object, median), round, round_dec)
  max <- lapply(lapply(object, max), round, round_dec)
  sd <- lapply(lapply(object, sd), round, round_dec)
  #sqrt <- lapply(lapply(n, sqrt), round, round_dec)
  se  <- lapply(lapply(object,sef), round, round_dec)
  #se  <- lapply(as.list(mapply("/",sqrt,sd)), round, round_dec)

  if (isTRUE(print)) {

    cat("\nSummary for stable isotope data object", "\n\n")

    if (!is.null(attributes(object)$consumer))
      cat("Community ", attributes(object)$community, "\n")
    if (!is.null(attributes(object)$consumer))
      cat("Consumer ", attributes(object)$consumer, "\n")
    if (!is.null(attributes(object)$baseline1))
      cat("Baseline 1 ", attributes(object)$baseline1, "\n")
    if (!is.null(attributes(object)$baseline2))
      cat("Baseline 2 ", attributes(object)$baseline2, "\n")
    cat("\n")
  }

  cbind(n,mean,sd,se,min,max,median)

}
