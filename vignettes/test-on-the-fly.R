# basic test script to work with tRophicPosition package as i build it.

library(tRophicPosition)

# Now we simulate some isotope data
data <- generateTPData()

#To do: Here we screen the data
#screenIsotopeData(data)

#Here we call the model without arguments i.e. will return a model with
#uninformative priors
model.string <- jagsOneBaseline()

#Or we can call the model with some arguments
model.string <- jagsOneBaseline(muBprior = "dnorm(0, 0.0001)")

#Or with other arguments
model.string <- jagsOneBaseline(muBprior = "dnorm(3, 0.0001)",
                                sigmaBprior = "dunif (0, 50)")

#Then we test the model with a full argument input
model.string <- jagsOneBaseline(muBprior = "dnorm(0, 0.0001)",
                                sigmaBprior = "dunif(0, 100)",
                                sigmaDeltaNprior = "dunif(0, 100)",
                                muDeltaNprior = "dnorm(0, 0.0001)",
                                sigmaPrior = "dunif(0, 100)",
                                TPprior = "dunif(lambda, 6)",
                                lambda = 2)

# Here we set up the model
model <- TPmodel(data = data,
                 model.string = model.string)

# and sample our parameters of interest
samp <- posteriorTP(model)

summary(samp)
plot(samp)
