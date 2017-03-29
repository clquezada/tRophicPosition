#' A function to generate random isotope data for trophic position calculation
#'
#' This function generates random isotope (d13C and d15N) data for using the
#' basic functions of tRophicPosition package.
#'
#' @param n.baselines Number of baselines (could be 1 or 2), by default is 2.
#' @param dNb1 mean value for delta Nitrogen of baseline 1. Default is a random
#' number between -5 and 5
#' @param dNc mean value for delta Nitrogen of secondary consumer. Default value
#' is dNb1 multiplied 2 times the trophic enrichment factor.
#' @param n.obsB Number of observations for baselines. Default is 25.
#' @param n.obsC Number of observations for consumer. Default is 25.
#' @param DeltaN mean value for trophic enrichment factor. Default value is 3.4
#' @param n.obsDeltaN Number of observations of deltaN (trophic enrichment
#' factor). Default is 56
#' @param DeltaC mean value for trophic enrichment factor. Default value is 0.39
#' @param n.obsDeltaC Number of observations of DeltaC (trophic enrichment
#' factor). Default is 107
#' @param sd.dNb1
#' @param dNb2
#' @param sd.dNb2
#' @param dCb1
#' @param sd.dCb1
#' @param dCb2
#' @param sd.dCb2
#' @param sd.dNc
#' @param dCc
#' @param sd.dCc
#' @param sd.DeltaN
#' @param sd.DeltaC
#' @param consumer
#'
#' @return A named list with dNb1, dNc and deltaN randomly generated
#' observations. If n.baselines = 2, then dCb1, dNb2, dCb2, dCc and deltaC are
#' also returned.
#' @export
#'
#' @examples

generateTPData <- function (n.baselines = 2,
                            n.obsB = 25,
                            n.obsC = 25,
                            dNb1 = NULL,
                            sd.dNb1 = 0.5,
                            dNb2 = NULL,
                            sd.dNb2 = 0.5,
                            dCb1 = NULL,
                            sd.dCb1 = 0.5,
                            dCb2 = NULL,
                            sd.dCb2 = 0.5,
                            dNc = NULL,
                            sd.dNc = 0.5,
                            dCc = NULL,
                            sd.dCc = 0.5,
                            n.obsDeltaN = 56,
                            DeltaN = 3.4,
                            sd.DeltaN = 0.98,
                            n.obsDeltaC = 107,
                            DeltaC = 0.39,
                            sd.DeltaC = 1.3,
                            consumer = NULL) {

  # Here we simulate some data for the first baseline
  # If dNb1 (dN of the baseline 1) is NULL, n.obsB random numbers are generated
  # within -5 and 5, with a standard deviation sd.dNb1
  if (is.null(dNb1)) dNb1 <- rnorm(n.obsB, runif(1, -5, 5), sd.dNb1)

  # If the user supply dNb1, then n.obsB numbers are randomly generated
  # from a normal distribution with mean dNb1 and standard deviation sd.dNb1
  else dNb1 <- rnorm(n.obsB, dNb1, sd.dNb1)

  # Now we simulate some data for the [secondary] consumer we want
  # to calculate trophic position of

  # If no dN of secondary consumer (dNc) is supplied, some data is
  # simulated two trophic positions over mean dN of baseline1
  if (is.null(dNc)) dNc <- rnorm(n.obsC, mean(dNb1) + (2 * DeltaN), sd.dNc)

  #If the user supply dNc, then n.obsC observations are randomly generated
  #from a normal distribution with mean dNc and standard deviation sd.dNc
  else dNc <- rnorm(n.obsC, dNc, sd.dNc)

  # Finally we simulate some data for the trophic enrichment factor (deltaN)
  # By default we generate 56 values randomly drawn from a normal distribution
  # with a mean 3.4 with sd 0.98 (defined in the arguments of this function)
  # deltaN <- rnorm (n.obsDeltaN, DeltaN, sd.DeltaN)
  deltaN <- simulateTEF(nN = n.obsDeltaN, meanN = DeltaN, sdN = sd.DeltaN)

  # We simulate then values for the dC of baseline 1 (with the same sd of dNb1)
  if (is.null(dCb1)) dCb1 <- rnorm(n.obsB, runif(1, -25, -10), sd.dNb1)
  else dCb1 <- rnorm(n.obsB, dCb1, sd.dNb1)

  # And values for dC of secondary consumer
  if (is.null(dCc)) dCc <- rnorm(n.obsC, runif(1, dCb1+2, 5), sd.dCc)
  else dCc <- rnorm(n.obsC, dCc, sd.dCc)

  if (n.baselines == 2){

    if (is.null(dNb2))
      dNb2 <- rnorm(n.obsB, runif(1, mean(dNb1)-DeltaN, mean(dNb1)+5), sd.dNb2)
    else
      dNb2 <- rnorm(n.obsB, dNb2, sd.dNb2)

    if (is.null(dCb2))
      dCb2 <- rnorm(n.obsB, runif(1, mean(dCc)+2, mean(dCc)+15), sd.dCb2)
    else
      dCb2 <- rnorm(n.obsB, dCb2, sd.dCb2)

    deltaC <- simulateTEF(nC = n.obsDeltaC, meanC = DeltaC, sdC = sd.DeltaC)

    a <- list(dNb1 = dNb1, dCb1 = dCb1,
              dNb2 = dNb2, dCb2 = dCb2,
              dNc = dNc, dCc = dCc,
              deltaN = deltaN, deltaC = deltaC)

    if (!is.null(consumer)) mostattributes(a) <- list(class = "isotopeData",
                                                      names = names(a),
                                                      consumer = consumer)
    else class(a) <- "isotopeData"

    return(a)

  } else if (n.baselines == 3){

#     dCb2 <- rnorm(n.obsB, runif(1, -25, -10), std.devB1)
#     dNb2 <- rnorm(n.obsB, runif(1, -5, 5), std.devB1)
#
#     dCb3 <- rnorm(n.obsB, runif(1, -25, -10), std.devB1)
#     dNb3 <- rnorm(n.obsB, runif(1, -5, 5), std.devB1)
#
#     return(list(dNb1 = dNb1, dCb1 = dCb1,
#                 dNc = dNc, dCc = dCc,
#                 dNb2 = dNb2, dCb2 = dCb2,
#                 dCb3 = dCb3, dNb3 = dNb3,
#                 deltaN = deltaN))
    stop("Three baselines not implemented yet")

  } else {

    return(list(dNb1 = dNb1, dCb1 = dCb1,
                dCc = dCc, dNc = dNc,
                deltaN = deltaN))

    }
}
