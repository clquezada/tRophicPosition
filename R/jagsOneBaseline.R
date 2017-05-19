#' Defines the jags model to fit the single baseline trophic position model
#'
#' Takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags]{jags.model}}. Although
#' it is possible to use a number of predefined or customized
#' distributions (see
#' \href{https://sourceforge.net/projects/mcmc-jags/files/Manuals/}{JAGS documentation}),
#'  it is likelly that most of the time
#' you will be using a normal distribution. This is the default option (i.e.
#' when the function is called without arguments) and it is like this:
#' "mu ~ dnorm(0, 0.0001)". In this case, a prior of normally distributed mu is
#' defined, with a mean 0, and a standard deviation of 0.0001. This is a normal
#' distributed prior, although uninformative. You might want to change the mean
#' and/or the standard deviation according to your previously knowledge of the
#' system you are working on. As well as the prior for mu, JAGS uses "tau",
#' which is the precision. Precision is a deterministic function (instead of the
#' distributional "~"), and it is calculated as "tau <- power(sigma, -2)", thus
#' you have to define as well sigma, which stands for the standard deviation.
#'
#' @param lambda an integer indicating the trophic position of the baseline.
#' @param muB a distribution defining prior for mean (mu) of baseline.
#' @param sigmaB a distribution defining sigma (std dev) of baseline.
#' @param muDeltaN a distribution defining prior for the mean (mu) of
#' deltaN. deltaN stands for trophic enrichment factor of Nitrogen.
#' @param sigmaDeltaN a distribution defining sigma (std dev) of deltaN.
#' @param TP a distribution defining prior of trophic position.
#' @param sigma a value defining sigma (std dev) of baseline.
#'
#' @return A jags model as a character string
#'
#' @export

jagsOneBaseline <- function (muB = NULL,
							sigmaB = NULL,
							muDeltaN = NULL,
							sigmaDeltaN = NULL,
							sigma = NULL,
							TP = NULL,
							lambda = NULL,
							...)
{

  # ----------------------------------------------------------------------------
  # JAGS code for fitting Inverse Wishart version of SIBER to a single group
  # ----------------------------------------------------------------------------

  modelString <- "

    model {
      # -----------------------------------------------------------------------
      # First we define all the likelihood functions

      # Likelihood for the baseline
      for (j in 1:length(dNb1)) {
        dNb1[j] ~ dnorm(muB, tauB)
        }

      # Likelihood for deltaN
      for (j in 1:length(deltaN)){
        deltaN[j] ~ dnorm(muDeltaN, tauDeltaN)
        }

      # And likelihood for the consumer
      # NB this is where trophic position TP
      # enters the model.
      for (i in 1:length(dNc)) {
        dNc[i] ~ dnorm(mu[i], tau)
        mu[i] <- muB + muDeltaN * (TP - lambda)
      }"

  # -----------------------------------------------------------------------
  # Now we define prior mean and precision for the baseline.
  # If muBprior doesn't exist, an uninformative prior is defined.
  # Otherwise muB is defined as the prior distribution
  # for muB
  if (is.null(muB)) {
    newString <- "muB ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muB ~", toString(muB))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # -----------------------------------------------------------------------
  # Here the process is repeated to define prior distribution for sigmaB
  if (is.null(sigmaB)) {
    newString <-     "tauB <- pow(sigmaB, -2)
                      sigmaB ~ dunif(0, 100)"
  } else {
    newString <- "tauB <- pow(sigmaB, -2)"
    newString2 <- paste("sigmaB ~", toString(sigmaB))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # -----------------------------------------------------------------------
  # Here the process is repeated to define prior mean and precision
  # for deltaN (trophic enrichment factor)
    if (is.null(muDeltaN)) {
    newString <-     "muDeltaN ~ dnorm(0, 0.0001)"
  } else {
    newString <- paste("muDeltaN ~", toString(muDeltaN))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # -----------------------------------------------------------------------
  # And now is repeated to define prior distribution for sigmaDeltaN
  if (is.null(sigmaDeltaN)) {
    newString <-     "tauDeltaN <- pow(sigmaDeltaN, -2)
                      sigmaDeltaN ~ dunif(0, 100)"
  } else {
    newString <- "tauDeltaN <- pow(sigmaDeltaN, -2)"
    newString2 <- paste("sigmaDeltaN ~", toString(sigmaDeltaN))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # -----------------------------------------------------------------------
  # And here it is defined prior precision for the consumer
  if (is.null(sigma)) {
    newString <-     "tau <- pow(sigma, -2)
                      sigma ~ dunif(0, 100)"
  } else {
    newString <- "tau <- pow(sigma, -2)"
    newString2 <- paste("sigma ~", toString(sigma))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # -----------------------------------------------------------------------
  # Here we define prior for Trophic Position (TP)
  if (is.null(TP)) {
    newString <-     "TP ~ dunif(lambda, 10)"
  } else {
    newString <- paste("TP ~ ", toString(TP))
  }

  modelString <- paste (modelString, newString, sep = "\n")


  # -----------------------------------------------------------------------
  # Finally we define lambda (i.e. trophic position of the baseline)
  if (is.null(lambda)) {
    newString <-     "lambda <- 2"
  } else {
    newString <- paste("lambda <-", toString(lambda))
  }

  modelString <- paste (modelString, newString, sep = "\n")


  newString <- "}" # end of jags model script
  modelString <- paste (modelString, newString, sep = "\n")

  return(modelString)

} # end of function
