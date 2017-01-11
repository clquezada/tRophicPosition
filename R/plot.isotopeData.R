#' Title
#'
#' @param isoData
#' @param consumer
#' @param b1
#' @param b2
#' @param legend
#' @param density
#'
#' @return
#' @export
#'
#' @examples
#'
plot.isotopeData <- function (IsotopeData, consumer = "Consumer",
                              b1 = "Pelagic baseline", b2 = "Benthic baseline",
                              legend = c(0.85, 0.85), density = "both") {

  screenIsotopeData(IsotopeData = IsotopeData, consumer = consumer , b1 = b1, b2 = b2,
                    legend = legend, density = density)


}
