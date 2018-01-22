#' Function to plot and screen stable isotope data with one or more baselines.
#'
#' This function receives a named list of vectors (isotopeData class object),
#' and plots a biplot with 2 sources and a consumer. The user can states whether
#' he/she wants a density function plotted above, to the right, at both sides or
#' does not want it to be plotted.
#'
#' @param isotopeData an isotopeData class object.
#' @param density string representing whether the density function is plotted.
#'   Accepted characters are "both" in which case will plot the density function
#'   above and to the right, "right", "above" or "none".
#' @param consumer string representing the consumer.
#' @param b1 string representing baseline 1.
#' @param b2 string representing baseline 2.
#' @param legend coordinates representing where to locate the legend.
#' @param title string representing title.
#' @param ... additional arguments passed to this function.
#'
#' @return none
#'
#' @export
#'
#' @examples
#' a <- generateTPData()
#' screenIsotopeData(a)

screenIsotopeData <- function (isotopeData = NULL, density = "both",
                               consumer = "Consumer", b1 = "Pelagic baseline",
                               b2 = "Benthic baseline", legend = c(1.15, 1.15),
                               title = NULL,
                               ...) {

  if (class(isotopeData) == "isotopeData")

    ifelse(is.null(title),
           screenIsotopeDataMoreSources(isotopeData, density, baselines = 2,
                                        consumer = consumer, b1 = b1, b2 = b2,
                                        legend = legend),
           screenIsotopeDataMoreSources(isotopeData, density, baselines = 2,
                                        consumer = consumer, b1 = b1, b2 = b2,
                                        legend = legend, title = title))

  else if (!is.null(isotopeData)) {
    #To do: isotopeData will be an object of the class IsoData. It would be a
    #great idea to have an object of some class, that can be used in every
    #stable isotopes R pkg. #Partially done in Nov 26 2016 (see above)
    #So, instead of checking the length of the list, we will check first if the
    #object has the required class. Maybe, the class will inform the length
    #(dimension) of the list and some other nice information. #Done in 0.6.8

    if (length(isotopeData) == 5) {

      screenIsotopeDataMoreSources(isotopeData, density, baselines = 1,
                                   consumer = consumer, ...)

    } else if (length(isotopeData) == 6) {

      screenIsotopeDataMoreSources(isotopeData, density, baselines = 1,
                                   consumer = consumer, ...)

    } else if (length(isotopeData) == 7) {

      screenIsotopeDataMoreSources(isotopeData, density, baselines = 2,
                                   consumer = consumer, b1 = b1, b2 = b2,
                                   legend = legend)

    } else if (length(isotopeData) == 8) {

      screenIsotopeDataMoreSources(isotopeData, density, baselines = 2,
                                   consumer = consumer, b1 = b1, b2 = b2,
                                   legend = legend)

    } else if (length(isotopeData) == 10){

      #To do...

      screenIsotopeDataMoreSources(isotopeData, density, baselines = 3,
                                   consumer = consumer)

    } else {warning("isotopeData doesn't have the correct dimension.
                    We expect to have length of 6, 8 or 10.")}

  } else {
    cat("You should call this function using isotopeData as argument.\n")
    cat(
      "If you don't have your own dataset, call first generateData() function.")
  }

}
