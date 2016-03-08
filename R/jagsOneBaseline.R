#' Defines the jags model to fit the single baseline trohpic position model
#'
#' Takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags.model]{rjags.model}}.
#'
#' @param lambda an integer indicating the trophic position of the baseline.
#' @param muBprior
#' @param sigmaBprior
#' @param muDeltaN
#' @param sigmaDeltaN
#' @param sigma
#' @param TP
#'
#' @return A jags model as a character string
#'
#' @export

jagsOneBaseline <- function (muBprior = NULL,
							sigmaBprior = NULL,
							muDeltaN = NULL,
							sigmaDeltaN = NULL,
							sigma = NULL,
							TP = NULL,
							lambda = NULL)
{

  # ----------------------------------------------------------------------------
  # JAGS code for fitting Inverse Wishart version of SIBER to a single group
  # ----------------------------------------------------------------------------

  modelstring <- "

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
  # If muBprior don't exist, an uninformative prior is defined.
  # Otherwise muBprior is defined as the prior distribution
  # for muB
  if (is.null(muBprior)) {
    newString <- "muB ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muB ~", toString(muBprior))
  }

  modelstring <- paste (modelstring, newString, sep = "\n")

  # Here the process is repeated to define prior distribution for sigmaB
  if (is.null(sigmaBprior)) {
    newString <-     "tauB <- pow(sigmaB, -2)
                      sigmaB ~ dunif(0, 100)"
  } else {
    newString <- "taub <- pow(sigmaB, -2)"
    newString2 <- paste("sigmaB ~", toString(sigmaBprior))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelstring <- paste (modelstring, newString, sep = "\n")

  # Here the process is repeated to define prior mean and precision
  # for deltaN (trophic enrichment factor)
    if (is.null(muDeltaN)) {
    newString <-     "muDeltaN ~ dnorm(0, 0.0001)"
  } else {
    newString <- paste("muDeltaN ~", toString(muDeltaN))
  }

  modelstring <- paste (modelstring, newString, sep = "\n")

  # And now is repeated to define prior distribution for sigmaDeltaN
  if (is.null(sigmaDeltaN)) {
    newString <-     "tauDeltaN <- pow(sigmaDeltaN, -2)
                      sigmaDeltaN ~ dunif(0, 100)"
  } else {
    newString <- "tauDeltaN <- pow(sigmaDeltaN, -2)"
    newString2 <- paste("sigmaDeltaN ~", toString(sigmaDeltaN))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelstring <- paste (modelstring, newString, sep = "\n")

  # And here it is defined prior precision for the consumer
  if (is.null(sigma)) {
    newString <-     "tau <- pow(sigma, -2)
                      sigma ~ dunif(0, 100)"
  } else {
    newString <- "tau <- pow(sigma, -2)"
    newString2 <- paste("sigma ~", toString(sigma))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelstring <- paste (modelstring, newString, sep = "\n")

  # Here we define prior for Trophic Position (TP)
  if (is.null(TP)) {
    newString <-     "TP ~ dunif(lambda, 10)"
  } else {
    newString <- paste("TP ~ ", toString(TP))
  }

  modelstring <- paste (modelstring, newString, sep = "\n")


  # Finally we define lambda (i.e. trophic position of the baseline)
  if (is.null(lambda)) {
    newString <-     "lambda <- 2"
  } else {
    newString <- paste("lambda <-", toString(lambda))
  }

  modelstring <- paste (modelstring, newString, sep = "\n")


  newString <- "}" # end of jags model script
  modelstring <- paste (modelstring, newString, sep = "\n")

  return(modelstring)

} # end of function
