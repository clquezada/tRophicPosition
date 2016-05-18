#' Function to plot and screen Isotope Data with one or more baselines.
#'
#' This function receives a named list of vectors, and plots a scatterplot with
#'  2 sources and a consumer. Depending on the length of the named list, this
#'  function plots one (length = 6), or two (length = 8) baselines. Also the
#'  user can states whether he/she wants a density function ploted above, to the
#'  right, both or does not want it to be ploted.
#'
#'
#' @param IsotopeData a named list composed at least of 6 vectors, dNb1, dCb1,
#' dCsc, dNsc, deltaN and deltaC.
#' dNb1 and dCb1 stands for delta Nitrogen and delta Carbon of baseline 1, dNsc
#' and dCsc stands for delta N and delta C of secondary consumer, and deltaN
#' and deltaC stands for the trophic enrichment factor (capital letter delta)
#' of Nitrogen and Carbon.
#' @param density a character that might plot the density function. Accepted
#' characters are "both" in which case will plot the density function above and
#' to the right, "right", "above" or "none".
#'
#' @return none
#'
#' @export
#'
#' @examples

screenIsotopeData <- function (IsotopeData = NULL, density = "both") {

  if (!is.null(IsotopeData)) {
    #To do: IsotopeData will be an object of the class IsoData. It would be a great
    #idea to have an object of some class, that can be used in every SIA R pkg.
    #So, instead of checking the length of the list, we will check first if the
    #object has the required class. Maybe, the class will inform the length
    #(dimension) of the list and some other nice information.
    if (length(IsotopeData) == 6) {

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 1)

    } else if (length(IsotopeData) == 8) {

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 2)

    } else if (length(IsotopeData) == 10){

      screenIsotopeDataMoreSources(IsotopeData, density, baselines = 3)

    } else {warning("IsotopeData doesn't have the correct dimension.
                    We expect to have length of 6, 8 or 10.")}

  } else {
    cat("You should call this function using IsotopeData as argument.\n")
    cat("If you don't have your own dataset, call first generateData() function.")
  }

}
