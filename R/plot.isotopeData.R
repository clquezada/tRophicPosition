#' Plot stable isotope data (2 elements) with one or two baselines
#'
#' @param isotopeData
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
plot.isotopeData <- function (isotopeData, consumer = NULL,
                              b1 = NULL, b2 = NULL,
                              legend = c(1.15, 1.15), density = "both") {

  if (is.null(b1) & is.null(attributes(isotopeData)$baseline1))
    b1 = "Pelagic baseline"
  else
    if(is.null(b1) & !is.null(attributes(isotopeData)$baseline1))
      b1 = attributes(isotopeData)$baseline1

  if (is.null(b2) & is.null(attributes(isotopeData)$baseline2))
    b2 = "Benthic baseline"
  else
    if(is.null(b2) & !is.null(attributes(isotopeData)$baseline2))
      b2 = attributes(isotopeData)$baseline2

  if (is.null(consumer) & is.null(attributes(isotopeData)$consumer))
    consumer = "Consumer"
  else
    if(!is.null(attributes(isotopeData)$consumer))
      consumer = attributes(isotopeData)$consumer


  if(is.null(attributes(isotopeData)$community))
    invisible(capture.output(screenIsotopeData(isotopeData = isotopeData,
                      consumer = consumer,
                      b1 = b1,
                      b2 = b2,
                      legend = legend,
                      density = density)))
    else
      invisible(capture.output(screenIsotopeData(isotopeData = isotopeData,
                        consumer = consumer,
                        b1 = b1,
                        b2 = b2,
                        legend = legend,
                        density = density,
                        title = attributes(isotopeData)$community)))
}
