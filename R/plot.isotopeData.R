#' Plot stable isotope data (2 elements) with one or two baselines
#'
#' @param x an isotopeData class object.
#' @param consumer string representing the consumer.
#' @param b1 string representing baseline 1.
#' @param b2 string representing baseline 2.
#' @param legend coordinates representing where to locate the legend.
#' @param density string representing whether the density function is plotted.
#' Accepted characters are "both" in which case will plot the density function
#' above and to the right, "right", "above" or "none".
#' @param ... additional arguments passed to this function.
#'
#' @return a ggplot2 object with the biplot of isotopes.
#' @export
#'
#' @examples
#' a <- generateTPData()
#' plot(a)
#'
plot.isotopeData <- function (x,
                              consumer = NULL,
                              b1 = NULL,
                              b2 = NULL,
                              legend = c(1.15, 1.15),
                              density = "both",
                              ...) {

  if (is.null(b1) & is.null(attributes(x)$baseline1))
    b1 = "Pelagic baseline"
  else
    if(is.null(b1) & !is.null(attributes(x)$baseline1))
      b1 = attributes(x)$baseline1

  if (is.null(b2) & is.null(attributes(x)$baseline2))
    b2 = "Benthic baseline"
  else
    if(is.null(b2) & !is.null(attributes(x)$baseline2))
      b2 = attributes(x)$baseline2

  if (is.null(consumer) & is.null(attributes(x)$consumer))
    consumer = "Consumer"
  else
    if(!is.null(attributes(x)$consumer))
      consumer = attributes(x)$consumer


  if(is.null(attributes(x)$community))
    invisible(utils::capture.output(screenIsotopeData(isotopeData = x,
                      consumer = consumer,
                      b1 = b1,
                      b2 = b2,
                      legend = legend,
                      density = density)))
    else
      invisible(utils::capture.output(screenIsotopeData(isotopeData = x,
                        consumer = consumer,
                        b1 = b1,
                        b2 = b2,
                        legend = legend,
                        density = density,
                        title = attributes(x)$community)))
}
