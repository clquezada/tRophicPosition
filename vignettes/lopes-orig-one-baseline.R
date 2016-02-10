#Trophic position R script - the simplest case with one baseline

library(rjags)
library(coda)
library(runjags)

#First we set up the working directory
setwd("D:/Dropbox (HarrodLab)/Articulos en prep/.R Package Trophic Position")

#Now we simulate some isotope data
#For the baseline1
dCb1 <- rnorm(25, -18, 2)
dNb1 <- rnorm(25, 0, 0.1)

#Now we simulate some data for the secondary consumer we want to calculate trophic position of
dCsc <- rnorm(25, -16, 1)
dNsc <- rnorm(25, 6.8, 0.1)

#Now we set up a model in JAGS syntaxis
model_string <- "
model {
for (j in 1:length(dNb1)) {
dNb1[j] ~ dnorm(mub, taub)
}

mub ~ dnorm(0, 0.0001)
taub <- pow(sigmab, -2)
sigmab ~ dunif(0, 100)


for (i in 1:length(dNsc)) {
dNsc[i] ~ dnorm(mu[i], tau)
mu[i] <- mub + deltaN * (TP - lambda)
}
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)

#prior for deltaN
#deltaN as constant
deltaN <- 3.4

#deltaN as normally distributed
#deltaN~ dnorm(3.4, 0.5)

#prior for TP: uninformative would be dnorm(0, 0.0001)
#TP ~ dnorm(0, 0.0001)
TP ~ dnorm(4, 0.1)
lambda <- 2

}"

#Here we set up the model
model <- jags.model(textConnection(model_string),
                    data = list(dNb1 = dNb1, dNsc = dNsc), n.chains = 4, n.adapt = 100000)

#Here we check if the model is correct
if (class(model) == "jags") {writeLines(model_string, "tr_oneBaseline.txt")}


#update the model
update(model, 100000)

#and sample our parameters of interest
samp <- (coda.samples(model,
                      variable.names=c("TP", "deltaN"),
                      n.iter=100000))
summary(samp)
plot(samp)

#Now we run the model using several cores, longer samples and more chains
runJagsOut <- run.jags(method="parallel", model="tr_oneBaseline.txt", monitor=c("mub", "sigmab", "TP"),
                       data = list(dNb1 = dNb1, dNsc = dNsc), n.chains = 4, adapt = 100000, burnin=100000, sample=500000,
                       thin=5, summarise=T, plots=T)
plot(runJagsOut)
runJagsOut
codaSamples = as.mcmc.list(runJagsOut)
