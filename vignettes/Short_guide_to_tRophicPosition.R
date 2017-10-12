## ----eval = FALSE--------------------------------------------------------
#  install.packages("tRophicPosition")

## ---- eval = FALSE-------------------------------------------------------
#  library(tRophicPosition)

## ----eval = FALSE--------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)

## ----eval = FALSE--------------------------------------------------------
#  install_github("clquezada/tRophicPosition", build_vignettes = TRUE)

## ------------------------------------------------------------------------
library(tRophicPosition)

## ------------------------------------------------------------------------
BilagayMEC <- read.csv(system.file("extdata", "Bilagay-MEC.csv",
                                   package = "tRophicPosition"))

## ------------------------------------------------------------------------
head(BilagayMEC)

## ------------------------------------------------------------------------
consumer <- loadIsotopeData(BilagayMEC, species = "Bilagay", speciesColumn = "FG",
                            b1 = "Pelagic_BL", b2 = "Benthic_BL",
                            baselineColumn = "FG",
                            community = "Coquimbo", communityColumn = "Location")

## ------------------------------------------------------------------------
# First we get TDF values from bibliography using McCutchan's et al (2003) paper
TDF_values <- TDF(author = "McCutchan", element = "both", type = "muscle")

# Then we use those values within the call to loadIsotopeData()
consumer_with_McCutchan <- loadIsotopeData(BilagayMEC, 
                                           species = "Bilagay",
                                           b1 = "Pelagic_BL",
                                           b2 = "Benthic_BL", 
                                           community = "Coquimbo",
                                           speciesColumn = "FG",
                                           baselineColumn = "FG",
                                           communityColumn = "Location",
                                           deltaN = TDF_values$deltaN,
                                           deltaC = TDF_values$deltaC)

## ---- fig.width = 6.5, fig.height = 5------------------------------------
# Here we explicitly include a label for both baseline 1 and baseline 2.
# We could change the label for the consumer (consumer = "new_label"), or even 
# change the position of the legend (legend = c(1.15, 1.15) is the default).
plot(consumer, b1 = "Pelagic baseline", b2 = "Benthic baseline")

## ------------------------------------------------------------------------
model.string <- jagsBayesianModel(model = "oneBaseline", TP = "dnorm(4, 0.1)")

## ----eval = FALSE--------------------------------------------------------
#  model <- TPmodel(data = consumer, model.string = model.string,
#                   n.adapt = 20000, n.chains = 2)

## ----echo = FALSE--------------------------------------------------------
model <- TPmodel(data = consumer, model.string = model.string,
                 n.adapt = 500, n.chains = 2)

## ----eval = FALSE--------------------------------------------------------
#  posterior.samples <- posteriorTP(model = model, n.iter = 20000,
#                                   variable.names = c("TP", "muDeltaN"))

## ----echo = FALSE--------------------------------------------------------
posterior.samples <- posteriorTP(model = model, n.iter = 500,
                                 variable.names = c("TP", "muDeltaN"))

posterior.samples <- tRophicPosition:::Bilagay_models$posterior.samples

## ---- fig.width = 6, fig.height = 5--------------------------------------
summary(posterior.samples)

## ------------------------------------------------------------------------
# First we combine both chains (if we sample them)
posterior.combined <- coda::mcmc(do.call(rbind, posterior.samples))

# Then we calculate the mode with this code
getPosteriorMode(posterior.combined)

## ---- fig.width = 6, fig.height = 6--------------------------------------
# First we install the package MCMCvis if we haven't done it before
# install.packages("MCMCvis")
 
# MCMCvis plots trace and density from posterior samples
MCMCvis::MCMCtrace(posterior.samples)

## ---- fig.width = 5, fig.height = 3.8------------------------------------
# MCMCplot visualize posterior distributions using caterpillar plots
MCMCvis::MCMCplot(posterior.samples, ylim = c(0,5), horiz = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  browseVignettes("tRophicPosition")

