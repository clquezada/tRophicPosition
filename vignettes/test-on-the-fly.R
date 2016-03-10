# basic test script to work with tRophicPosition package as i build it.

library(tRophicPosition)

# Now we simulate some isotope data
# You can specify how many baselines (though not working yet: n.baselines),
# how many observations per baseline (n.obsB) and secondary consumer (n.obsSC),
# mean for dN of baseline 1 (dNb1), mean for dN of secondary consumer (dNsc),
# mean for trophic enrichment factor (deltaN), and standard deviation for
# each dNb1 (std.devB), dNsc (std.devSC) and deltaN (std.devDeltaN).
# Additionally the user can specify number of observations for deltaN (n.obsDeltaN).
data <- generateTPData()

# Here we screen the data
# For now it is just a basic histogram, although more fancy plots are planned.
screenIsotopeData(data)

#Here we call the model without arguments i.e. will return a model with
#uninformative priors
model.string <- jagsOneBaseline()

#Or we can call the model with one argument defining a prior distribution
model.string <- jagsOneBaseline(muBprior = "dnorm(0, 0.0001)")

#Or with more arguments
model.string <- jagsOneBaseline(muBprior = "dnorm(3, 0.0001)",
                                sigmaBprior = "dunif (0, 50)")

#Or we can test the model with a full argument input
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
