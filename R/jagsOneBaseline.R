#' Defines the jags model to fit the single baseline trohpic position model
#'
#' Takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags.model]{rjags.model}}.
#'
#' @param lambda an integer indicating the trophic position of the baseline.
#' @param muBprior a distribution defining prior for mu of Baseline.
#' @param muDeltaNprior
#' @param sigmaDeltaNprior
#' @param sigmaPrior
#' @param TPprior
#' @param sigmaBprior
#'
#' @return A jags model as a character string
#'
#' @export

jagsOneBaseline <- function (muBprior = NULL,
							sigmaBprior = NULL,
							muDeltaNprior = NULL,
							sigmaDeltaNprior = NULL,
							sigmaPrior = NULL,
							TPprior = NULL,
							lambda = NULL)
{

  # ----------------------------------------------------------------------------
  # JAGS code for fitting Inverse Wishart version of SIBER to a single group
  # ----------------------------------------------------------------------------

  modelString <- "

    model {
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
      for (i in 1:length(dNsc)) {
        dNsc[i] ~ dnorm(mu[i], tau)
        mu[i] <- muB + muDeltaN * (TP - lambda)
      }"

  # Now we define prior mean and precision for the baseline.
  # If muBprior doesn't exist, an uninformative prior is defined.
  # Otherwise muBprior is defined as the prior distribution
  # for muB
  if (is.null(muBprior)) {
    newString <- "muB ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muB ~", toString(muBprior))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # Here the process is repeated to define prior distribution for sigmaB
  if (is.null(sigmaBprior)) {
    newString <-     "tauB <- pow(sigmaB, -2)
                      sigmaB ~ dunif(0, 100)"
  } else {
    newString <- "tauB <- pow(sigmaB, -2)"
    newString2 <- paste("sigmaB ~", toString(sigmaBprior))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # Here the process is repeated to define prior mean and precision
  # for deltaN (trophic enrichment factor)
    if (is.null(muDeltaNprior)) {
    newString <-     "muDeltaN ~ dnorm(0, 0.0001)"
  } else {
    newString <- paste("muDeltaN ~", toString(muDeltaNprior))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # And now is repeated to define prior distribution for sigmaDeltaN
  if (is.null(sigmaDeltaNprior)) {
    newString <-     "tauDeltaN <- pow(sigmaDeltaN, -2)
                      sigmaDeltaN ~ dunif(0, 100)"
  } else {
    newString <- "tauDeltaN <- pow(sigmaDeltaN, -2)"
    newString2 <- paste("sigmaDeltaN ~", toString(sigmaDeltaNprior))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # And here it is defined prior precision for the consumer
  if (is.null(sigmaPrior)) {
    newString <-     "tau <- pow(sigma, -2)
                      sigma ~ dunif(0, 100)"
  } else {
    newString <- "tau <- pow(sigma, -2)"
    newString2 <- paste("sigma ~", toString(sigmaPrior))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # Here we define prior for Trophic Position (TP)
  if (is.null(TPprior)) {
    newString <-     "TP ~ dunif(lambda, 10)"
  } else {
    newString <- paste("TP ~ ", toString(TPprior))
  }

  modelString <- paste (modelString, newString, sep = "\n")


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
