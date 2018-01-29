#' Defines a jags Bayesian model to fit a single baseline trophic position model
#'
#' This function takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags]{jags.model}}.
#'
#' The single baseline trophic position model is defined as:
#'
#' \deqn{dNc = dNb + deltaN * (TP - lambda)}
#'
#' where dNc are d15N values of consumer, dNb1 are d15N values of baseline,
#' deltaN is the trophic discrimination factor for N, TP is trophic position of
#' the consumer and lambda is the trophic level of baseline. Furthermore, as a
#' Bayesian approach, dNb, deltaN and dNc are defined as random parameters with
#' a normal distribution with mean mu_i and precision tau_i, TP is a random
#' parameter with a uniform distribution and lambda is a constant. All these
#' distributions can be changed modifying them as priors, while defining lambda
#' within the call to the function.
#'
#' Although it is possible to use a number of predefined or customized
#' distributions (see distribution aliases in
#' \href{https://sourceforge.net/projects/mcmc-jags/files/Manuals/}{JAGS
#' documentation}), it is likely that most of the time you will be using a
#' normal distribution as prior for most parameters. This is the default option
#' (i.e. when the function is called without arguments). To change it, you need
#' to indicate a mean and standard deviation for the parameter of interest, for
#' example "dnorm(0, 0.0001)". Here, a prior of normally distributed mu is
#' defined, with a mean 0, and a standard deviation of 0.0001. This constitutes
#' a normally distributed prior, although uninformative. You might want to
#' change the mean and/or the standard deviation according to your prior
#' knowledge of the system/consumer you are working on. As well as the priors
#' for mu, JAGS uses "tau", which is the precision for defining the standard
#' deviation of mu. Precision is a deterministic function (instead of the
#' distributional "~"), and it is calculated as "tau <- power(sigma, -2)", thus
#' you could define as well sigma_i, which stands for the standard deviation of
#' the parameter of interest.
#'
#' @param lambda an integer indicating the trophic level of the baseline.
#'   Default is 2.
#' @param muB a distribution defining prior for mean (mu) of baseline. By
#'   default is dnorm(0, 0.0001).
#' @param sigmaB a distribution defining sigma (standard deviation) of baseline.
#'   By default is dunif(0, 100).
#' @param muDeltaN a distribution defining prior for the mean (mu) of deltaN.
#'   deltaN stands for trophic discrimination factor of Nitrogen. By default is
#'   dnorm(0, 0.0001).
#' @param sigmaDeltaN a distribution defining sigma (standard deviation) of
#'   deltaN. By default is dunif(0, 100).
#' @param TP a distribution defining prior of trophic position. By default is
#'   dunif(lambda, 10), with lambda = 2 if no defined before.
#' @param sigma a value defining sigma (standard deviation) of baseline. By
#'   default is dunif(0, 100).
#' @param ... additional arguments passed to jagsOneBaseline.
#'
#' @return A jags model (BUGS-language) as a character string
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

  ##########################
  ## Check priors
  ##########################

  arg <- do.call(cbind, (as.list(match.call())[-1]))
  colnames <- colnames(arg)
  count <- 0
  for (i in seq_along(arg)) {

    if(colnames[i] == "lambda") next()

    if(grepl("dnorm(", arg[i], fixed = TRUE) &
       grepl(",", arg[i], fixed = TRUE) &
       grepl(")", arg[i], fixed = TRUE)) {
      next()
    } else if (grepl("dunif(", arg[i], fixed = TRUE) &
               grepl(",", arg[i], fixed = TRUE) &
               grepl(")", arg[i], fixed = TRUE)) {
      next()
    } else if (grepl("dbeta(", arg[i], fixed = TRUE) &
               grepl(",", arg[i], fixed = TRUE) &
               grepl(")", arg[i], fixed = TRUE)) {
      next()
    }
    count <- count + 1
  }

  Check <- ArgumentCheck::newArgCheck()

  if (count > 0)
    ArgumentCheck::addWarning(
      msg = "It seems that you are not using dnorm(mean, sd),  dunif(min, max)
   or dbeta(a, b) as priors, or they are not correctly written. Please check
   the arguments.",
      argcheck = Check
    )
  ArgumentCheck::finishArgCheck(Check)

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
      }

      for (i in 1:length(dNc)) {
        dNcPred[i] ~ dnorm(mu[i], tau)
      }
      #Prediction <- sum(dNcPred)

  "

  # -----------------------------------------------------------------------
  # Now we define prior mean and precision for the baseline.
  # If muB doesn't exist, an uninformative prior is defined.
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
    if(!is.numeric(lambda)) stop("lambda must be numeric")
    newString <- paste("lambda <-", toString(lambda))
  }

  modelString <- paste (modelString, newString, sep = "\n")


  newString <- "}" # end of jags model script
  modelString <- paste (modelString, newString, sep = "\n")

  class(modelString) <- append(class(modelString), "oneBaseline")

  return(modelString)

} # end of function
