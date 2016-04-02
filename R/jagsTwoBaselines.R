#' Defines the jags model to fit the two baseline trophic position model
#'
#' Takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags.model]{rjags.model}}.
#'
#' @param TPprior
#' @param mubprior
#'
#' @return A jags model as a character string
#'
#' @export

jagsTwoBaselines <- function (TPprior = NULL,
                              mubprior = NULL)
{

  # ----------------------------------------------------------------------------
  # JAGS code for fitting Inverse Wishart version of SIBER to two groups
  # ----------------------------------------------------------------------------

  modelstring <- "

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

  # Likelihood for dC of secondary consumer (dCsc) is a simple mixing model of
  # the dC of the two baselines

  #dCsc is modelled as having a normal distribution
  #with mean calculated with the two baselines weighted by alpha
  for (i in 1:length(dCsc)) {
  dCsc[i] ~ dnorm(alpha * (muCb1 - muCb2) + muCb2, tauCsc)
  }

  # ----------------------------------------------------------------------------
  # Likelihood for the nitrogen data in the consumer uses the estimated
  # proportion of baseline 1 and 2 in the consumer to inform trophic position.
  for (i in 1:length(dNsc)){
  dNsc[i] ~ dnorm(muDeltaN * (TP - lambda) + muNb1*alpha + muNb2 * (1 - alpha), tauNsc)
  }

  # ----------------------------------------------------------------------------
  # Priors for dCb1
  muCb1 ~ dnorm(0, 0.0001)
  #tau (precision) is deterministic with sigma raised to -2
  tauCb1 <- pow(sigmaCb1, -2)
  #sigma is stochastic with an uninformative prior between 0 and 100
  sigmaCb1 ~ dunif(0, 100)

  # ----------------------------------------------------------------------------
  # Priors for dNb1
  muNb1 ~ dnorm(0, 0.0001)
  tauNb1 <- pow(sigmaNb1, -2)
  sigmaNb1 ~ dunif(0, 100)

  # ----------------------------------------------------------------------------
  # Priors for dCb2
  muCb2 ~ dnorm(0, 0.0001)
  tauCb2 <- pow(sigmaCb2, -2)
  sigmaCb2 ~ dunif(0, 100)

  # ----------------------------------------------------------------------------
  # Priors for dNb2
  muNb2 ~ dnorm(0, 0.0001)
  tauNb2 <- pow(sigmaNb2, -2)
  sigmaNb2 ~ dunif(0, 100)

  # ----------------------------------------------------------------------------
  # Priors on the carbon mixing model
  alpha ~ dbeta(1,1) # the proportion of baseline 1 in the consumer.
  tauCsc <- pow(sigmaCsc, -2)
  sigmaCsc ~ dunif(0, 100)

  # ----------------------------------------------------------------------------
  # Priors on the dN in the consumer
  tauNsc <- pow(sigmaNsc, -2)
  sigmaNsc ~ dunif(0, 100)
  TP ~ dunif(lambda, 10)

  # ----------------------------------------------------------------------------
  # Priors on the deltaN (trophic enrichment factor)
  muDeltaN ~ dnorm(0, 0.0001)
  tauDeltaN <- pow(sigmaDeltaN, -2)
  sigmaDeltaN ~ dunif(0, 100)

  #constants
  lambda <- 2

}" # end of jags model script

  return(modelstring)

} # end of function
