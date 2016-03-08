# basic test script to work with tRophicPosition package as i build it.

library(tRophicPosition)

# Now we simulate some isotope data
# or the baseline1

dCb1 <- rnorm(25, -18, 2)
dNb1 <- rnorm(25, 0, 0.1)

# Now we simulate some data for the secondary consumer we want
# to calculate trophic position of

dCsc <- rnorm(25, -16, 1)
dNsc <- rnorm(25, 6.8, 0.1)

# Finally we simulate some deltaN observed data
deltaN <- rnorm (20, 3.4, 0.1)

# Here we call the model without arguments i.e. will return a model with
# uninformative priors
model.string <- jagsOneBaseline()

#Here we call the model with some arguments
model.string <- jagsOneBaseline(muBprior = "dnorm(0, 0.0001)")

#And so on...
model.string <- jagsOneBaseline(muBprior = "dnorm(0, 0.0001)",
                                sigmaBprior = "dunif (0, 100)")

#Then we test the model with a full argument input
model.string <- jagsOneBaseline(muBprior = "dnorm(0, 0.0001)",
                                sigmaBprior = "dunif(0, 100)",
                                sigmaDeltaNprior = "dunif(0, 100)",
                                muDeltaNprior = "dnorm(0, 0.0001)",
                                sigmaPrior = "dunif(0, 100)",
                                TPprior = "dunif(lambda, 6)",
                                lambda = 2)

# Here we set up the model
model <- rjags::jags.model(textConnection(model.string),
                           data = list(dNb1 = dNb1, dNsc = dNsc, deltaN = deltaN),
                           n.chains = 2,
                           n.adapt = 10000)

# and sample our parameters of interest
samp <- rjags::coda.samples(model,
                            variable.names=c("TP", "muDeltaN"),
                            n.iter = 10000)
summary(samp)
plot(samp)
