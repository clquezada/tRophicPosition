#' Defines the jags model to fit the two baseline trophic position model
#'
#' Takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags.model]{rjags.model}}.
#'
#' @param muCb1
#' @param sigmaCb1
#' @param muNb1
#' @param sigmaNb1
#' @param muCb2
#' @param sigmaCb2
#' @param muNb2
#' @param sigmaNb2
#' @param alpha
#' @param sigmaCsc
#' @param TP
#' @param sigmaNsc
#' @param muDeltaN
#' @param sigmaDeltaN
#' @param lambda
#'
#' @return A jags model as a character string
#'
#' @export

jagsTwoBaselinesFull <- function (muCb1 = NULL,
                              sigmaCb1 = NULL,
                              muNb1 = NULL,
                              sigmaNb1 = NULL,
                              muCb2 = NULL,
                              sigmaCb2 = NULL,
                              muNb2 = NULL,
                              sigmaNb2 = NULL,
                              alpha = NULL,
                              sigmaCsc = NULL,
                              TP = NULL,
                              sigmaNsc = NULL,
                              muDeltaN = NULL,
                              sigmaDeltaN = NULL,
                              muDeltaC = NULL,
                              sigmaDeltaC = NULL,
                              lambda = NULL)
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

  # Likelihood for deltaC
  for (j in 1:length(deltaC)){
  deltaC[j] ~ dnorm(muDeltaC, tauDeltaC)
  }

  # ----------------------------------------------------------------------------
  #And now we are ready to calculate the trophic position
  # ----------------------------------------------------------------------------

  # Likelihood for dC of secondary consumer (dCsc) is a simple mixing model of
  # the dC of the two baselines

  #dCsc is modelled as having a normal distribution
  #with mean calculated with the two baselines weighted by alpha
  for (i in 1:length(dCsc)) {
  dCsc[i] ~ dnorm(muCb2 + muDeltaC - (alpha * (muCb2 - muCb1)), tauCsc)
  }

  # ----------------------------------------------------------------------------
  # Likelihood for the nitrogen data in the consumer uses the estimated
  # proportion of baseline 1 and 2 in the consumer to inform trophic position.
  for (i in 1:length(dNsc)){
  dNsc[i] ~ dnorm(muDeltaN * (TP - lambda) + muNb1*alpha + muNb2 * (1 - alpha), tauNsc)
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

  if (is.null(sigmaCsc)) {
    newString <-     "tauCsc <- pow(sigmaCsc, -2)
    sigmaCsc ~ dunif(0, 100)"
  } else {
    newString <- "tauCsc <- pow(sigmaCsc, -2)"
    newString2 <- paste("sigmaCsc ~", toString(sigmaCsc))
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

  if (is.null(sigmaNsc)) {
    newString <-     "tauNsc <- pow(sigmaNsc, -2)
    sigmaNsc ~ dunif(0, 100)"
  } else {
    newString <- "tauNsc <- pow(sigmaNsc, -2)"
    newString2 <- paste("sigmaNsc ~", toString(sigmaNsc))
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
    newString <- paste("lambda ~", toString(lambda))
  }
  modelString <- paste (modelString, newString, sep = "\n")

  newString <- "}" # end of jags model script
  modelString <- paste (modelString, newString, sep = "\n")

  return(modelString)

  } # end of function
