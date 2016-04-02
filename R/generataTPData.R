
#' A function to generate random data for trophic position calculation
#'
#' This function generates random data for using the tRophicPosition basic
#' functions.
#'
#' @param n.baselines Number of baselines (could be 1 or 2)
#' @param dNb1 mean value for delta Nitrogen of baseline 1. Default is a random
#' number between -5 and 5
#' @param dNsc mean value for delta Nitrogen of secondary consumer. Default value
#' is dNb1 multiplied 2 times the trophic enrichment factor.
#' @param deltaN mean value for trophic enrichment factor
#' @param n.obsB Number of observations for baseline. Default is 25.
#' @param n.obsSC Number of observations for secondary consumer Default is 25.
#' @param std.devB1 Standard deviation for randomly generated observations for
#' baseline. Default is 0.1
#' @param std.devSC Standard deviation for randomly generated observations for
#' secondary consumer. Default is 0.1
#' @param n.obsDeltaN Number of observations of deltaN (trophic enrichment factor).
#' Default is 20.
#' @param std.devDeltaN Standard deviation for randomly generated observations for
#' deltaN. Default is 0.1
#'
#' @return A named list with dNb1, dNsc and deltaN randomly generated
#' observations. If n.baselines = 2, then dCb1, dNb2, dCb2 and dCsc are also
#' returned.
#' @export
#'
#' @examples

generateTPData <- function (n.baselines = 1,
                            n.obsB = 25,
                            n.obsSC = 25,
                            dNb1 = NULL,
                            std.devB1 = 0.1,
                            dNsc = NULL,
                            std.devSC = 0.1,
                            n.obsDeltaN = 20,
                            deltaN = 3.4,
                            std.devDeltaN = 0.1) {

  # Here we simulate some data for the first baseline
  # If dNb1 (dN of the baseline 1) is NULL, n.obsB random numbers are generated
  # within -5 and 5, with a standard deviation std.dev
  if (is.null(dNb1)){
    dNb1 <- rnorm(n.obsB, runif(1, -5, 5), std.devB1)
  } else {
    # If the user supply dNb1, then n.obsB numbers are randomly generated
    # from a normal distribution with mean dNb1 and standard deviation std.devB1
    dNb1 <- rnorm(n.obsB, dNb1, std.devB1)
  }

  # Now we simulate some data for the [secondary] consumer we want
  # to calculate trophic position of

  # If no dN of secondary consumer (dNsc) is supplied, some data is
  # simulated two trophic positions over mean dN of baseline1
  if (is.null(dNsc)) {
    dNsc <- rnorm(n.obsSC, mean(dNb1) + (2 * deltaN), std.devSC)
  } else {
    #If the user supply dNsc, then n.obsSC observations are randomly generated
    #from a normal distribution with mean dNsc and standard deviation std.devSC
    dNsc <- rnorm(n.obsSC, dNsc, std.devSC)
  }

  # Finally we simulate some data for the trophic enrichment factor (deltaN)
  # By default we generate 20 values randomly drawn from a normal distribution
  # with a mean 3.4 with stdev 0.1
  deltaN <- rnorm (n.obsDeltaN, deltaN, std.devDeltaN)

  if (n.baselines == 2){

    dCb1 <- rnorm(n.obsB, runif(1, -25, -10), std.devB1)

    dNb2 <- rnorm(n.obsB, runif(1, -5, 5), std.devB1)
    dCb2 <- rnorm(n.obsB, runif(1, -25, -10), std.devB1)

    dCsc <- rnorm(n.obsSC, runif(1, -25, -10), std.devSC)

    return(list(dNb1 = dNb1, dCb1 = dCb1, dNsc = dNsc, dCsc = dCsc,
                deltaN = deltaN, dNb2 = dNb2, dCb2 = dCb2))

  } else {

    return(list(dNb1 = dNb1, dNsc = dNsc, deltaN = deltaN))

    }
}
