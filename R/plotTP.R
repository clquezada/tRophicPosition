#' Function to plot a trophic position distribution
#'
#' Wrapper of {\link[SIBER]{siberDensityPlot}}.
#'
#' @param TPdist One posterior distribution (or a collection) of trophic position.
#' In case of wanting to plot two or more posterior distributions, needs to be
#' passed as a {\link[base]{data.frame}} object.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'species1 <- stats::rnorm(1000, 4, 0.1)
#'species2 <- stats::rnorm(1000, 3, 0.8)
#'plotTP(data.frame(species1, species2))
#'
#'

plotTP <- function (TPdist = NULL, ...) {

  SIBER::siberDensityPlot(TPdist, ylab = "Trophic Position", ...)

}
