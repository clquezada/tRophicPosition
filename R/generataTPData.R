#'A function to generate random stable isotope data for trophic position
#'calculation
#'
#'This function generates random stable isotope (d13C and d15N) data to use
#'basic functions and calculations coded within the package.
#'
#'@param n.baselines number of baselines (could be 1 or 2), default is 2.
#'@param n.obsB number of observations for baselines. Default is 25.
#'@param dNb1 mean value for d15N of baseline 1. Default is a random number
#'  between -5 and 5.
#'@param sd.dNb1 standard deviation for d15N of baseline 1.
#'@param dCb1 mean value for d13C of baseline 1.
#'@param sd.dCb1 standard deviation for d13C of baseline 1.
#'@param dNb2 mean value for d15N of baseline 2.
#'@param sd.dNb2 standard deviation for d15N of baseline 2.
#'@param dCb2 mean value for d13C of baseline 2.
#'@param sd.dCb2 standard deviation for d13C of baseline 2.
#'@param consumer string for consumer.
#'@param n.obsC number of observations for consumer. Default is 25.
#'@param dNc mean value for d15N of consumer. Default value is dNb1 multiplied 2
#'  times the trophic discrimination factor.
#'@param sd.dNc standard deviation for d15N of consumer.
#'@param dCc mean value for d13C of consumer.
#'@param sd.dCc standard deviation for d13C of consumer.
#'@param DeltaN mean value for trophic discrimination factor of nitrogen.
#'  Default value is 3.4.
#'@param sd.DeltaN standard deviation for trophic discrimination factor of
#'  nitrogen. Default value is 0.98.
#'@param n.obsDeltaN number of observations of deltaN (trophic discrimination
#'  factor). Default value is 56.
#'@param DeltaC mean value for trophic discrimination factor of carbon. Default
#'  value is 0.39.
#'@param sd.DeltaC standard deviation for trophic discrimination factor for
#'  carbon. Default value is 1.3.
#'@param n.obsDeltaC number of observations of DeltaC (trophic discrimination
#'  factor). Default is 107.
#'@param seed numerical value to get reproducible results.
#'
#'@return An isotopeData class object (named list) with dNb1, dNc and deltaN
#'  randomly generated observations. If n.baselines = 2, then dCb1, dNb2, dCb2,
#'  dCc and deltaC are also returned.
#'@export
#'
#' @examples
#' ## Good data
#' a <-generateTPData(dCb1 = -10, dNb1 = -10,
#' dCc = -4, dNc = 4,
#' dCb2 = 2, dNb2 = 0)
#' plot(a)
#'
#' ## Consumer more enriched in carbon
#' b <-generateTPData(dCb1 = -10, dCc = 0, dCb2 = -2)
#' plot(b)
#'
#' ## Consumer much more enriched
#' c <-generateTPData(dCb1 = -10, dCc = 3, dCb2 = -2)
#' plot(c)

generateTPData <- function (n.baselines = 2,
                            n.obsB = 25,
                            dNb1 = NULL,
                            sd.dNb1 = 1,
                            dCb1 = NULL,
                            sd.dCb1 = 1,
                            dNb2 = NULL,
                            sd.dNb2 = 1,
                            dCb2 = NULL,
                            sd.dCb2 = 1,
                            n.obsC = 25,
                            consumer = NULL,
                            dNc = NULL,
                            sd.dNc = 1,
                            dCc = NULL,
                            sd.dCc = 1,
                            DeltaN = 3.4,
                            sd.DeltaN = 0.98,
                            n.obsDeltaN = 56,
                            DeltaC = 0.39,
                            sd.DeltaC = 1.3,
                            n.obsDeltaC = 107,
                            seed = 3) {
  set.seed(seed)

  meanSD <- function(x, mean, sd) {

    x <- stats::rnorm(x, mean, sd)
    X <- x
    MEAN <- mean
    SD <- sd
    Z <- (((X - mean(X, na.rm = TRUE))/sd(X, na.rm = TRUE))) * SD
    MEAN + Z
  }

  # Here we simulate some data for the first baseline
  # If dNb1 (dN of the baseline 1) is NULL, n.obsB random numbers are generated
  # within -5 and 5, with a standard deviation sd.dNb1
  if (is.null(dNb1)) dNb1 <- meanSD(n.obsB, stats::runif(1, -5, 5), sd.dNb1)

  # If the user supply dNb1, then n.obsB numbers are randomly generated
  # from a normal distribution with mean dNb1 and standard deviation sd.dNb1
  else dNb1 <- meanSD(n.obsB, dNb1, sd.dNb1)

  # Now we simulate some data for the [secondary] consumer we want
  # to calculate trophic position of

  # If no dN of secondary consumer (dNc) is supplied, some data is
  # simulated two trophic positions over mean dN of baseline1
  if (is.null(dNc)) dNc <- meanSD(n.obsC, mean(dNb1) + (2 * DeltaN), sd.dNc)

  #If the user supply dNc, then n.obsC observations are randomly generated
  #from a normal distribution with mean dNc and standard deviation sd.dNc
  else dNc <- meanSD(n.obsC, dNc, sd.dNc)

  # Finally we simulate some data for the trophic discrimination factor (deltaN)
  # By default we generate 56 values randomly drawn from a normal distribution
  # with a mean 3.4 with sd 0.98 (defined in the arguments of this function)
  # deltaN <- meanSD (n.obsDeltaN, DeltaN, sd.DeltaN)
  deltaN <- simulateTDF(nN = n.obsDeltaN, meanN = DeltaN, sdN = sd.DeltaN)

  # We simulate then values for the dC of baseline 1 (with sd.Cb1)
  if (is.null(dCb1)) dCb1 <- meanSD(n.obsB, stats::runif(1, -25, -10), sd.dCb1)
  else dCb1 <- meanSD(n.obsB, dCb1, sd.dCb1)

  # And values for dC of secondary consumer
  if (is.null(dCc)) dCc <- meanSD(n.obsC, stats::runif(1, dCb1+2, 5), sd.dCc)
  else dCc <- meanSD(n.obsC, dCc, sd.dCc)

  if (n.baselines == 2){

    if (is.null(dNb2))
      dNb2 <- meanSD(n.obsB, stats::runif(1, mean(dNb1)-DeltaN, mean(dNb1)+5),
                     sd.dNb2)
    else
      dNb2 <- meanSD(n.obsB, dNb2, sd.dNb2)

    if (is.null(dCb2))
      dCb2 <- meanSD(n.obsB, stats::runif(1, mean(dCc)+2, mean(dCc)+15),
                     sd.dCb2)
    else
      dCb2 <- meanSD(n.obsB, dCb2, sd.dCb2)

    deltaC <- simulateTDF(nC = n.obsDeltaC, meanC = DeltaC, sdC = sd.DeltaC)

    a <- list(dNb1 = dNb1, dCb1 = dCb1,
              dNb2 = dNb2, dCb2 = dCb2,
              dNc = dNc, dCc = dCc,
              deltaN = deltaN, deltaC = deltaC)

    if (!is.null(consumer)) mostattributes(a) <- list(class = "isotopeData",
                                                      names = names(a),
                                                      consumer = consumer)
    else class(a) <- "isotopeData"

    return(a)

  } else if (n.baselines > 2){

    #     dCb2 <- rnorm(n.obsB, stats::runif(1, -25, -10), std.devB1)
    #     dNb2 <- rnorm(n.obsB, stats::runif(1, -5, 5), std.devB1)
    #
    #     dCb3 <- rnorm(n.obsB, stats::runif(1, -25, -10), std.devB1)
    #     dNb3 <- rnorm(n.obsB, stats::runif(1, -5, 5), std.devB1)
    #
    #     return(list(dNb1 = dNb1, dCb1 = dCb1,
    #                 dNc = dNc, dCc = dCc,
    #                 dNb2 = dNb2, dCb2 = dCb2,
    #                 dCb3 = dCb3, dNb3 = dNb3,
    #                 deltaN = deltaN))
    stop("Three or more baselines is not implemented yet")

  } else {

    return(list(dNb1 = dNb1, dCb1 = dCb1,
                dCc = dCc, dNc = dNc,
                deltaN = deltaN))

  }
}
