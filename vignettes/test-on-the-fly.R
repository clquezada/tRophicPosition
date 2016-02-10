# basic test script to work with tRophicPosition package as i build it.

library(tRophicPosition)

#Now we simulate some isotope data
#For the baseline1
dCb1 <- rnorm(25, -18, 2)
dNb1 <- rnorm(25, 0, 0.1)

#Now we simulate some data for the secondary consumer we want to calculate trophic position of
dCsc <- rnorm(25, -16, 1)
dNsc <- rnorm(25, 6.8, 0.1)

model.string <- jagsOneBaseline()


#Here we set up the model
model <- rjags::jags.model(textConnection(model.string),
                           data = list(dNb1 = dNb1, dNsc = dNsc),
                           n.chains = 2,
                           n.adapt = 100000)

# AJ _ not sure we need this.
#Here we check if the model is correct
#if (class(model) == "jags") {writeLines(model_string, "tr_oneBaseline.txt")}


# AJ - not sure this is necessary
#update the model
#update(model, 10000)

#and sample our parameters of interest
samp <- rjags::coda.samples(model,
                            variable.names=c("TP", "deltaN"),
                            n.iter = 10000)
summary(samp)
plot(samp)

# AJ - do we want to run it in parallel using the other code?
