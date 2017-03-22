#' Summary for stable isotope data
#'
#' @param siData
#'
#' @return
#' @export
#'
#' @examples
summary.isotopeData <- function (siData, print = TRUE, round_dec = 1) {

  if (class(siData) != "isotopeData") stop("The object don't have a class isotopeData")

  #class(isotopeData) <- "list"

  #To Do: do.call(rbind, lapply(my.list, data.frame, stringsAsFactors=FALSE))
  #
  sef <- function(x) sd(x)/sqrt(length(x))

  n <- lapply(siData, length)
  min <- lapply(lapply(siData, min), round, round_dec)
  mean <- lapply(lapply(siData, mean), round, round_dec)
  median <- lapply(lapply(siData, median), round, round_dec)
  max <- lapply(lapply(siData, max), round, round_dec)
  sd <- lapply(lapply(siData, sd), round, round_dec)
  #sqrt <- lapply(lapply(n, sqrt), round, round_dec)
  se  <- lapply(lapply(siData,sef), round, round_dec)
  #se  <- lapply(as.list(mapply("/",sqrt,sd)), round, round_dec)

  if (isTRUE(print)) {

    cat("\nSummary for stable isotope data object", "\n\n")

    if (!is.null(attributes(siData)$consumer))
      cat("Community ", attributes(siData)$community, "\n")
    if (!is.null(attributes(siData)$consumer))
      cat("Consumer ", attributes(siData)$consumer, "\n")
    if (!is.null(attributes(siData)$baseline1))
      cat("Baseline 1 ", attributes(siData)$baseline1, "\n")
    if (!is.null(attributes(siData)$baseline2))
      cat("Baseline 2 ", attributes(siData)$baseline2, "\n")
    cat("\n")
  }

  cbind(n,mean,sd,se,min,max,median)

}
