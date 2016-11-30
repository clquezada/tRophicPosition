#' Function to plot and screen isotope data with one or more baselines.
#'
#' This function receives a named list of vectors, and plots a scatterplot with
#'  2 sources and a consumer. Depending on the length of the named list, this
#'  function plots one (length = 6), or two (length = 8) baselines. Also the
#'  user can states whether he/she wants a density function ploted above, to the
#'  right, both or does not want it to be ploted.
#'
#'
#' @param IsotopeData
#' @param density a character that might plot the density function. Accepted
#' characters are "both" in which case will plot the density function above and
#' to the right, "right", "above" or "none".
#' @param consumer
#' @param b1
#' @param b2
#' @param legend
#' @param ...
#'
#' @return none
#'
#' @export
#'
#' @examples

screenIsotopeData <- function (IsotopeData = NULL, density = "both",
                               consumer = "Consumer", b1 = "Pelagic baseline",
                               b2 = "Benthic baseline", legend = c(0.85, 0.85),
                               ...) {

  if (class(IsotopeData) == "isotopeData")

    screenIsotopeDataMoreSources(IsotopeData, density, baselines = 2,
                                 consumer = consumer, b1 = b1, b2 = b2,
                                 legend = legend)

  else if (!is.null(IsotopeData)) {
    #To do: IsotopeData will be an object of the class IsoData. It would be a
    #great idea to have an object of some class, that can be used in every
    #stable isotopes R pkg. #Partially done in Nov 26 2016 (see above)
    #So, instead of checking the length of the list, we will check first if the
    #object has the required class. Maybe, the class will inform the length
    #(dimension) of the list and some other nice information.

    if (length(IsotopeData) == 5) {

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 1,
                                   consumer = consumer, ...)

    } else if (length(IsotopeData) == 6) {

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 1,
                                   consumer = consumer, ...)

    } else if (length(IsotopeData) == 7) {

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 2,
                                   consumer = consumer, b1 = b1, b2 = b2,
                                   legend = legend)

    } else if (length(IsotopeData) == 8) {

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 2,
                                   consumer = consumer, b1 = b1, b2 = b2,
                                   legend = legend)

    } else if (length(IsotopeData) == 10){

      #To do...

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 3,
                                   consumer = consumer)

    } else {warning("IsotopeData doesn't have the correct dimension.
                    We expect to have length of 6, 8 or 10.")}

  } else {
    cat("You should call this function using IsotopeData as argument.\n")
    cat("If you don't have your own dataset, call first generateData() function.")
  }

}
