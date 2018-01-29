#' Defines a jags Bayesian model to fit a two baselines trophic position full
#' model (with fractionation for C)
#'
#' Takes some parameters and returns a jags model object as a character string
#' for passing to \code{\link[rjags]{jags.model}}.
#'
#' The two baselines trophic position full model is defined as:
#'
#' \deqn{dNc ~ dnorm(deltaN * (TP - lambda) + dNb1*alpha + dNb2 * (1 - alpha),
#' tauNc)} and \deqn{dCc ~ dnorm(dCb2 + (deltaC * (TP-lambda)) + (alpha * (dCb1
#' - dCb2)), tauCc))}
#'
#' where dNc and dCc are d15N and d13C values of consumer, dNb1 and dCb1 are
#' d15N and d13C values of baseline 1, dNb2 and dCb2 are d15N and d13C values of
#' baseline 2, alpha is the relative proportion of N derived from baseline 1,
#' deltaN is the trophic discrimination factor for N, deltaC is the trophic
#' discrimination factor for C, TP is trophic position of the consumer and
#' lambda is the trophic level of baselines.
#'
#' In this Bayesian model, both dNc and dCc are modelled as having a normal
#' distribution with means calculated with above equations and precision (tauNc
#' and tauCc) calculated as standard deviation ^-2. Furthermore, dNb1, dCb1,
#' dNb2, dCb2, deltaN and deltaC are defined as random parameters with a normal
#' distribution with mean mu_i and precision tau_i, TP is a random parameter
#' with a uniform distribution, alpha is a random parameter with a beta
#' distribution and lambda is a constant. All these distributions can be changed
#' modifying them as priors, while defining lambda within the call to the
#' function.
#'
#' You might want to change the mean, standard deviation or other parameters of
#' the distributions according to your prior knowledge of the system/consumer
#' you are working on. Although it is possible to use a number of predefined or
#' customized distributions (see distribution aliases in
#' \href{https://sourceforge.net/projects/mcmc-jags/files/Manuals/}{JAGS
#' documentation}), it is likely that most of the time you will be using a
#' normal distribution as prior for most parameters. This is the default option
#' (i.e. when the function is called without arguments). To change it, you need
#' to indicate a mean and standard deviation for the i-est parameter of
#' interest, for example "dnorm(0, 0.0001)". Here, a prior of normally
#' distributed mu_i is defined, with a mean 0, and a standard deviation of
#' 0.0001. This constitutes an uninformative and normally distributed prior, for
#' the mean of the i-est parameter.  As well as the priors for mu_i, JAGS uses
#' "tau", which is the precision for defining the standard deviation of mu_i.
#' Precision is a deterministic function (instead of the distributional "~"),
#' and it is calculated as "tau_i <- power(sigma_i, -2)", thus you could define
#' as well sigma_i, which stands for the standard deviation of the i-est
#' parameter of interest. In the case of alpha, the default is a beta
#' distribution with parameters a = 1 and b = 1.

#'
#' @param sigmaNc a distribution defining sigma (standard deviation) for N of
#'   consumer. Default is dunif(0, 100).
#' @param sigmaCc a distribution defining sigma (standard deviation) for C of
#'   consumer. Default is dunif(0, 100).
#' @param muCb1 a distribution defining prior for mean (mu) for C of baseline 1.
#'   Default is dnorm(0, 0.0001).
#' @param sigmaCb1 a distribution defining sigma (standard deviation) for C of
#'   baseline 1. Default is dunif(0, 100).
#' @param muNb1 a distribution defining prior for mean (mu) for N of baseline 1.
#'   dnorm(0, 0.0001)
#' @param sigmaNb1 a distribution defining sigma (standard deviation) for N of
#'   baseline 1. Default is dunif(0, 100).
#' @param muCb2 a distribution defining prior for mean (mu) for C of baseline 2.
#'   dnorm(0, 0.0001)
#' @param sigmaCb2  a distribution defining sigma (standard deviation) for C of
#'   baseline 2. Default is dunif(0, 100).
#' @param muNb2 a distribution defining prior for mean (mu) for N of baseline 2.
#'   dnorm(0, 0.0001)
#' @param sigmaNb2 a distribution defining sigma (standard deviation) for N of
#'   baseline 2. Default is dunif(0, 100).
#' @param alpha a distribution defining alpha (mixing model between 2 sources).
#'   Default is dbeta(1,1).
#' @param lambda an integer indicating the trophic position of the baseline.
#'   Default is 2.
#' @param TP a distribution defining prior of trophic position. Default is
#'   dunif(lambda, 10), with lambda defined above.
#' @param muDeltaN a distribution defining prior for the mean (mu) of deltaN,
#'   which stands for trophic discrimination factor of Nitrogen. Default is
#'   dnorm(0, 0.0001).
#' @param sigmaDeltaN a value defining sigma (standard deviation) for the mean
#'   (mu) of deltaN. Default is dunif(0, 100).
#' @param muDeltaC a distribution defining prior for the mean (mu) of deltaC,
#'   which stands for trophic discrimination factor of Carbon
#' @param sigmaDeltaC a value defining sigma (standard deviation) for the mean
#'   (mu) of deltaC.
#' @param ... additional arguments passed to this function.
#'
#' @return A jags model as a character string
#'
#' @export


jagsTwoBaselinesFull <- function (sigmaNc = NULL,
                                  sigmaCc = NULL,
                                  muCb1 = NULL,
                                  sigmaCb1 = NULL,
                                  muNb1 = NULL,
                                  sigmaNb1 = NULL,
                                  muCb2 = NULL,
                                  sigmaCb2 = NULL,
                                  muNb2 = NULL,
                                  sigmaNb2 = NULL,
                                  lambda = NULL,
                                  TP = NULL,
                                  alpha = NULL,
                                  muDeltaN = NULL,
                                  sigmaDeltaN = NULL,
                                  muDeltaC = NULL,
                                  sigmaDeltaC = NULL,
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
  # JAGS code for fitting Inverse Wishart version of SIBER to two groups
  # ----------------------------------------------------------------------------

  modelString <- "

  model {
  # -----------------------------------------------------------------------
  # First we define all the likelihood functions
  # Likelihood for dC of baseline 1 (normally distributed)
  for (i in 1:length(dCb1)){
  dCb1[i] ~ dnorm(muCb1, tauCb1)
  }

  # The same is repeated for dN of baseline 1
  for (i in 1:length(dNb1)){
  dNb1[i] ~ dnorm(muNb1, tauNb1)
  }

  # And is also repeated for dN of baseline 2
  for (i in 1:length(dNb2)){
  dNb2[i] ~ dnorm(muNb2, tauNb2)
  }

  # The same is repeated for dC of baseline 2
  for (i in 1:length(dCb2)){
  dCb2[i] ~ dnorm(muCb2, tauCb2)
  }

  # Likelihood for deltaN
  for (j in 1:length(deltaN)){
  deltaN[j] ~ dnorm(muDeltaN, tauDeltaN)
  }

  # Likelihood for deltaC
  for (j in 1:length(deltaC)){
  deltaC[j] ~ dnorm(muDeltaC, tauDeltaC)
  }

  # ----------------------------------------------------------------------------
  #And now we are ready to calculate the trophic position
  # ----------------------------------------------------------------------------

  # Likelihood for dC of consumer (dCc) is a simple mixing model of
  # the dC of the two baselines

  # dCc is modelled as having a normal distribution
  # with mean calculated with the two baselines weighted by alpha
  for (i in 1:length(dCc)) {
  dCc[i] ~ dnorm(muCc[i], tauCc)
  muCc[i] <- muCb2 + (muDeltaC * (TP-lambda)) + (alpha * (muCb1 - muCb2))
  }

  # ----------------------------------------------------------------------------
  # Likelihood for the nitrogen data in the consumer uses the estimated
  # proportion of baseline 1 and 2 in the consumer to inform trophic position.
  for (i in 1:length(dNc)){
  dNc[i] ~ dnorm(muNc[i], tauNc)
  muNc[i] <- muDeltaN * (TP - lambda) + muNb1*alpha + muNb2 * (1 - alpha)
  }

  # Likelihood for the predicted nitrogen data
  for (i in 1:length(dNc)) {
    dNcPred[i] ~ dnorm(muNc[i], tauNc)
  }

  # Likelihood for the predicted carbon data
  for (i in 1:length(dCc)) {
    dCcPred[i] ~ dnorm(muCc[i], tauCc)
  }

  "


  # ----------------------------------------------------------------------------
  # Priors
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Priors for dCb1
  if (is.null(muCb1)) {
    newString <- "muCb1 ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muCb1 ~", toString(muCb1))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaCb1)) {
    newString <-     "tauCb1 <- pow(sigmaCb1, -2)
    sigmaCb1 ~ dunif(0, 100)"
  } else {
    newString <- "tauCb1 <- pow(sigmaCb1, -2)"
    newString2 <- paste("sigmaCb1 ~", toString(sigmaCb1))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # ----------------------------------------------------------------------------
  # Priors for dNb1
  if (is.null(muNb1)) {
    newString <- "muNb1 ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muNb1 ~", toString(muNb1))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaNb1)) {
    newString <-     "tauNb1 <- pow(sigmaNb1, -2)
    sigmaNb1 ~ dunif(0, 100)"
  } else {
    newString <- "tauNb1 <- pow(sigmaNb1, -2)"
    newString2 <- paste("sigmaNb1 ~", toString(sigmaNb1))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")


  # ----------------------------------------------------------------------------
  # Priors for dCb2
  if (is.null(muCb2)) {
    newString <- "muCb2 ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muCb2 ~", toString(muCb2))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaCb2)) {
    newString <-     "tauCb2 <- pow(sigmaCb2, -2)
    sigmaCb2 ~ dunif(0, 100)"
  } else {
    newString <- "tauCb2 <- pow(sigmaCb2, -2)"
    newString2 <- paste("sigmaCb2 ~", toString(sigmaCb2))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # ----------------------------------------------------------------------------
  # Priors for dNb2
  if (is.null(muNb2)) {
    newString <- "muNb2 ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muNb2 ~", toString(muNb2))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaNb2)) {
    newString <-     "tauNb2 <- pow(sigmaNb2, -2)
    sigmaNb2 ~ dunif(0, 100)"
  } else {
    newString <- "tauNb2 <- pow(sigmaNb2, -2)"
    newString2 <- paste("sigmaNb2 ~", toString(sigmaNb2))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")


  # ----------------------------------------------------------------------------
  # Priors on the carbon mixing model
  if (is.null(alpha)) {
    newString <- "alpha ~ dbeta(1,1)"

  } else {
    newString <- paste("alpha ~", toString(alpha))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaCc)) {
    newString <-     "tauCc <- pow(sigmaCc, -2)
    sigmaCc ~ dunif(0, 100)"
  } else {
    newString <- "tauCc <- pow(sigmaCc, -2)"
    newString2 <- paste("sigmaCc ~", toString(sigmaCc))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # ----------------------------------------------------------------------------
  # Priors on the dN in the consumer
  if (is.null(TP)) {
    newString <- "TP ~ dunif(lambda, 10)"

  } else {
    newString <- paste("TP ~", toString(TP))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaNc)) {
    newString <-     "tauNc <- pow(sigmaNc, -2)
    sigmaNc ~ dunif(0, 100)"
  } else {
    newString <- "tauNc <- pow(sigmaNc, -2)"
    newString2 <- paste("sigmaNc ~", toString(sigmaNc))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # ----------------------------------------------------------------------------
  # Priors on the deltaN (Nitrogen trophic enrichment factor)
  if (is.null(muDeltaN)) {
    newString <- "muDeltaN ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muDeltaN ~", toString(muDeltaN))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaDeltaN)) {
    newString <-     "tauDeltaN <- pow(sigmaDeltaN, -2)
    sigmaDeltaN ~ dunif(0, 100)"
  } else {
    newString <- "tauDeltaN <- pow(sigmaDeltaN, -2)"
    newString2 <- paste("sigmaDeltaN ~", toString(sigmaDeltaN))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # ----------------------------------------------------------------------------
  # Priors on the deltaC (Carbon trophic enrichment factor)
  if (is.null(muDeltaC)) {
    newString <- "muDeltaC ~ dnorm(0, 0.0001)"

  } else {
    newString <- paste("muDeltaC ~", toString(muDeltaC))
  }

  modelString <- paste (modelString, newString, sep = "\n")

  if (is.null(sigmaDeltaC)) {
    newString <-     "tauDeltaC <- pow(sigmaDeltaC, -2)
    sigmaDeltaC ~ dunif(0, 100)"
  } else {
    newString <- "tauDeltaC <- pow(sigmaDeltaC, -2)"
    newString2 <- paste("sigmaDeltaC ~", toString(sigmaDeltaC))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")


  #constants
  if (is.null(lambda)) {
    newString <- "lambda <- 2"

  } else {
    if(!is.numeric(lambda)) stop("lambda must be numeric")
    newString <- paste("lambda <- ", toString(lambda))
  }
  modelString <- paste (modelString, newString, sep = "\n")

  newString <- "}" # end of jags model script
  modelString <- paste (modelString, newString, sep = "\n")

  class(modelString) <- append(class(modelString), "twoBaselinesFull")

  return(modelString)

  } # end of function
