#' Defines the jags model to fit the two baselines trophic position model
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
#' @param muCb1 a distribution defining prior for mean (mu) for C of baseline 1.
#' @param sigmaCb1 a distribution defining sigma (std dev) for C of baseline 1.
#' @param muNb1 a distribution defining prior for mean (mu) for N of baseline 1.
#' @param sigmaNb1  a distribution defining sigma (std dev) for N of baseline 1.
#' @param muCb2 a distribution defining prior for mean (mu) for C of baseline 2.
#' @param sigmaCb2  a distribution defining sigma (std dev) for C of baseline 2.
#' @param muNb2 a distribution defining prior for mean (mu) for N of baseline 2.
#' @param sigmaNb2 a distribution defining sigma (std dev) for N of baseline 2.
#' @param alpha  a distribution defining alpha (mixing model between 2 sources).
#' @param sigmaCc a distribution defining sigma (std dev) for C of consumer.
#' @param TP a distribution defining prior of trophic position.
#' @param sigmaNc a distribution defining sigma (std dev) for N of consumer.
#' @param muDeltaN a distribution defining prior for the mean (mu) of
#' deltaN. deltaN stands for trophic enrichment factor of Nitrogen.
#' @param sigmaDeltaN a value defining sigma (std dev) for the mean (mu) of
#' deltaN.
#' @param lambda an integer indicating the trophic position of the baseline.
#'
#' @return A jags model as a character string
#'
#' @export

jagsTwoBaselines <- function (muCb1 = NULL,
                              sigmaCb1 = NULL,
                              muNb1 = NULL,
                              sigmaNb1 = NULL,
                              muCb2 = NULL,
                              sigmaCb2 = NULL,
                              muNb2 = NULL,
                              sigmaNb2 = NULL,
                              alpha = NULL,
                              sigmaCc = NULL,
                              TP = NULL,
                              sigmaNc = NULL,
                              muDeltaN = NULL,
                              sigmaDeltaN = NULL,
                              lambda = NULL,
                              ...)
{

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

  # ----------------------------------------------------------------------------
  #And now we are ready to calculate the trophic position
  # ----------------------------------------------------------------------------

  # Likelihood for dC of consumer (dCc) is a simple mixing model of
  # the dC of the two baselines

  #dCc is modelled as having a normal distribution
  #with mean calculated with the two baselines weighted by alpha
  for (i in 1:length(dCc)) {
  dCc[i] ~ dnorm(alpha * (muCb1 - muCb2) + muCb2, tauCsc)
  }

  # ----------------------------------------------------------------------------
  # Likelihood for the nitrogen data in the consumer uses the estimated
  # proportion of baseline 1 and 2 in the consumer to inform trophic position.
  for (i in 1:length(dNc)){
  dNc[i] ~ dnorm(muDeltaN * (TP - lambda) + muNb1*alpha + muNb2 * (1 - alpha), tauNsc)
  }"

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
    newString <-     "tauCsc <- pow(sigmaCc, -2)
                      sigmaCc ~ dunif(0, 100)"
  } else {
    newString <- "tauCsc <- pow(sigmaCc, -2)"
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
    newString <-     "tauNsc <- pow(sigmaNc, -2)
                      sigmaNc ~ dunif(0, 100)"
  } else {
    newString <- "tauNsc <- pow(sigmaNc, -2)"
    newString2 <- paste("sigmaNc ~", toString(sigmaNc))
    newString <- paste(newString, newString2, sep = "\n")
  }

  modelString <- paste (modelString, newString, sep = "\n")

  # ----------------------------------------------------------------------------
  # Priors on the deltaN (trophic enrichment factor)
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


  #constants
  if (is.null(lambda)) {
    newString <- "lambda <- 2"

  } else {
    newString <- paste("lambda <- ", toString(lambda))
  }
  modelString <- paste (modelString, newString, sep = "\n")

  newString <- "}" # end of jags model script
  modelString <- paste (modelString, newString, sep = "\n")

  return(modelString)

} # end of function
