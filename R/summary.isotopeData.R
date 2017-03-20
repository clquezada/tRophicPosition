#' Summary for stable isotope data
#'
#' @param siData
#'
#' @return
#' @export
#'
#' @examples
summary.isotopeData <- function (siData, print = TRUE) {

  if (class(siData) != "isotopeData") stop("The object don't have a class isotopeData")

  #class(isotopeData) <- "list"

  #To Do: do.call(rbind, lapply(my.list, data.frame, stringsAsFactors=FALSE))
  #
  sef <- function(x) sd(x)/sqrt(length(x))

  n <- lapply(siData, length)
  min <- lapply(lapply(siData, min), round, 3)
  mean <- lapply(lapply(siData, mean), round, 3)
  median <- lapply(lapply(siData, median), round, 3)
  max <- lapply(lapply(siData, max), round, 3)
  sd <- lapply(lapply(siData, sd), round, 3)
  #sqrt <- lapply(lapply(n, sqrt), round, 1)
  se  <- lapply(lapply(siData,sef), round, 3)
  #se  <- lapply(as.list(mapply("/",sqrt,sd)), round, 1)

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
