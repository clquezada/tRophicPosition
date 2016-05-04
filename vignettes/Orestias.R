library(tRophicPosition)
library(ggplot2)

#Here we load the Orestias and Rainbow trout data
data(Orestias_trout)

head(Orestias_trout)

ggplot(data = Orestias_trout, aes(d13C, d15N, shape = FG, colour = Species)) +
  geom_point(size = 3) + theme(legend.position = "none") +
  theme_bw() + ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))

#Orestias data is organized as having each variable its own column
data(Orestias)
head(Orestias)

IsotopeData <- lapply(Orestias, na.omit)

screenIsotopeData(IsotopeData)

model.string <- jagsTwoBaselines()

# Here we set up the model
model <- TPmodel(data = IsotopeData,
                 model.string = model.string,
                 n.chains = 2,
                 n.adapt = 50000)

# and sample our parameters of interest
samp <- posteriorTP(model, c("TP", "muDeltaN", "alpha"))

summary(samp)
plot(samp)

#Here we save the mcmc sampled posterior trophic position (only first chain)
Orestias.TP <- as.data.frame(samp[[1]][,"TP"])$var1

#And then we combine it with the posterior samples from the second chain
Orestias.TP <- c(Orestias.TP,as.data.frame(samp[[2]][,"TP"])$var1)


##################
data(Trout)

IsotopeData <- lapply(Trout, na.omit)

screenIsotopeData(IsotopeData)

model.string <- jagsTwoBaselines()

# Here we set up the model
model <- TPmodel(data = IsotopeData,
                 model.string = model.string,
                 n.chains = 2)

# and sample our parameters of interest
samp <- posteriorTP(model, c("TP", "muDeltaN", "alpha"))

summary(samp)
plot(samp)

#And then combine the posterior TP samples from the two chains
Trout.TP <- as.data.frame(samp[[1]][,"TP"])$var1
Trout.TP <- c(Trout.TP,as.data.frame(samp[[2]][,"TP"])$var1)

##################
#Orestias vs trout

#Now we build a data frame combining both TP and creating a Species factor
TP <- c(Orestias.TP, Trout.TP)
Species <- c(rep("Orestias chungarensis", length(Orestias.TP)),
             rep("Onchorhynchus mykiss", length(Trout.TP)))

df <- data.frame(TP, Species)

# Here we use the function for plotting trophic position grouped without
# quantiles (that is the default)
trophicDensityPlot(df)

#Or we can add quantiles
trophicDensityPlot(df, quantiles = TRUE)

#Or we can plot one species separated from the another
# (by default this plot flip the coordinates)
trophicDensityPlot(df, grouped = FALSE)

#Or we can add quantiles and separate the plot per the Species factor
trophicDensityPlot(df, quantiles = TRUE, grouped = FALSE)

#Or we can borrow from siber the density plot function ;)
SIBER::siberDensityPlot(data.frame(Orestias.TP, Trout.TP))

#And finally, we test for differences in a Bayesian way
compareTwoDistributions(Orestias.TP, Trout.TP, "<=")
