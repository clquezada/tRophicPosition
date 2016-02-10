#' Defines the jags model to fit the single baseline trohpic position model
#'
#' Takes no input parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags.model]{rjags.model}}.
#'
#' @return A jags model as a character string
#'
#'  @export

jagsOneBaseline <- function ()
{

  # ----------------------------------------------------------------------------
  # JAGS code for fitting Inverse Wishart version of SIBER to a single group
  # ----------------------------------------------------------------------------

  modelstring <- "

    model {

      # Likelihood for the baseline
      for (j in 1:length(dNb1)) {
        dNb1[j] ~ dnorm(mub, taub)
        }

      # Prior mean and precision for the baselines
      mub ~ dnorm(0, 0.0001)
      taub <- pow(sigmab, -2)
      sigmab ~ dunif(0, 100)

      # Likelihood for the consumer
      # NB this is where trophic position TP
      # enters the model.
      for (i in 1:length(dNsc)) {
        dNsc[i] ~ dnorm(mu[i], tau)
        mu[i] <- mub + deltaN * (TP - lambda)
      }

      # Prior precision for the consumer
      tau <- pow(sigma, -2)
      sigma ~ dunif(0, 100)

      # prior for TP: uninformative would be dnorm(0, 0.0001)
      # AJ - should this probably be strictly positive?
      # maybe dunif(lambda, 6) or something? In reality, we can probably
      # be quite informative about this prior.
      # I would model this as dunif() and let the user specify the
      # range as an input.
      #TP ~ dnorm(0, 0.0001)
      TP ~ dnorm(4, 0.1)

      # lambda is the trophic position of the baseline species
      # AJ - i would let the user include this as an input.
      lambda <- 2

      # -------------------------------------------------------
      # NB we have an option here. Trophic fractionation is
      # either a constant, or has a distribution. Ulimately
      # this is more reasonably a sum of random normal distributions
      # with one distribution per trophic level above the source.
      # Such a feature complicates things somewhat since it is not
      # an integer value, and the correction to the variance would be
      # entirely dependent on whether these distributions are
      # independent or not. More thought needed, but this is a
      # very reasonable starting point.
      #
      # prior for deltaN
      # deltaN as constant
      deltaN <- 3.4

      #deltaN as normally distributed
      #deltaN~ dnorm(3.4, 0.5)
  }" # end of jags model script

  return(modelstring)

} # end of function
