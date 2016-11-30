#' Summary for stable isotope data
#'
#' @param siData
#'
#' @return
#' @export
#'
#' @examples
summary.isotopeData <- function (siData) {

  if (class(siData) != "isotopeData") stop("The object don't have a class isotopeData")

  #class(isotopeData) <- "list"

  #To Do: do.call(rbind, lapply(my.list, data.frame, stringsAsFactors=FALSE))

  n <- lapply(lapply(siData, length), round, 2)
  min <- lapply(lapply(siData, min), round, 2)
  mean <- lapply(lapply(siData, mean), round, 2)
  median <- lapply(lapply(siData, median), round, 2)
  max <- lapply(lapply(siData, max), round, 2)
  sd <- lapply(lapply(siData, sd), round, 2)
  sqrt <- lapply(lapply(n, sqrt), round, 2)
  se  <- lapply(as.list(mapply("/",sqrt,sd)), round, 2)

  cbind(n,min,max,mean,median,sd,se)

}
