#' Function to plot and screen Isotope Data with one or two baselines.
#'
#' This function receives a named list of vectors, and plot a basic
#' histogram if the list has 3 elements. If it receives a list of 7 elements,
#' calls another function to plot a scatterplot with 2 sources and a consumer.
#'
#' @param IsotopeData a named list composed of two vectors, dNb1 and dNsc.
#' dNb1 stands for delta Nitrogen of baseline 1, while dNsc stands for
#' delta Nitrogen of secondary consumer.
#'
#' @return none
#' @export
#'
#' @examples

screenIsotopeData <- function (IsotopeData = NULL) {

  if (!is.null(IsotopeData)) {
    #To do: IsotopeData will be an object of the class IsoData. It would be a great
    #idea to have an object of some class, that can be used in every SIA R pkg.
    #So, instead of checking the length of the list, we will check first if the
    #object has the required class. Maybe, the class will inform the length
    #(dimension) of the list and some other nice information.
    if (length(IsotopeData) == 3) {

      screenIsotopeData1source(IsotopeData)

    }
    if (length(IsotopeData) == 7) {

      screenIsotopeData2sources(IsotopeData)

    }
  } else {
    cat("You should call this function using IsotopeData as argument.\n")
    cat("If you don't have your own dataset, call first generateData() function.")
  }

}
