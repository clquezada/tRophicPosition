# basic test script to work with tRophicPosition package as i build it.

library(tRophicPosition)

# Now we simulate some isotope data
# You can specify how many baselines (n.baselines),
# how many observations per baseline (n.obsB) and secondary consumer (n.obsSC),
# mean for dN of baseline 1 (dNb1), mean for dN of secondary consumer (dNsc),
# mean for trophic enrichment factor (deltaN), and standard deviation for
# each dNb1 (std.devB), dNsc (std.devSC) and deltaN (std.devDeltaN).
# Additionally the user can specify number of observations for deltaN (n.obsDeltaN).
data <- generateTPData()

# Here we screen the data
# When there is one baseline it is just a basic histogram,
# although more fancy plots are planned.
screenIsotopeData(data)

#We can generate data with two baselines as well
data <- generateTPData(n.baselines = 2, std.devB1 = 0.5, std.devSC = 0.5)

#In that case, the plot is a scatterplot with density plots for each dN and dC
screenIsotopeData(data)

#Here we check the data
head(data)

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

#In case we have two baselines
model.string <- jagsTwoBaselines()


# Here we set up the model
model <- TPmodel(data = data,
                 model.string = model.string)

# and sample our parameters of interest
samp <- posteriorTP(model, c("TP", "muDeltaN", "alpha"))

summary(samp)
plot(samp)
