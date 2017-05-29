## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)

## ---- eval = FALSE-------------------------------------------------------
#  install_github("clquezada/tRophicPosition")

## ------------------------------------------------------------------------
library(tRophicPosition)

## ------------------------------------------------------------------------
consumer1 <- generateTPData(dCb1 = -17.3, dNb1 = 14.2,
                            dCc = -15, dNc = 21,
                            dCb2 = -12.7, dNb2 = 15.4,
                            DeltaN = 3.4, sd.DeltaN = 0.98,
                            DeltaC = 0.39, sd.DeltaC = 1.3,
                            consumer = "Consumer 1")

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot(consumer1)

## ---- eval = FALSE-------------------------------------------------------
#  consumer1_models <- multiModelTP(consumer1)

## ---- eval = FALSE, echo = FALSE-----------------------------------------
#  consumer1_models <- multiModelTP(consumer1)
#  Bilagay_models <- tRophicPosition:::Bilagay_models
#  Bilagay_models$consumer1_models <- consumer1_models
#  Bilagay_models$consumer1_models$samples <- NULL
#  devtools::use_data(Bilagay_models, internal = TRUE, overwrite = TRUE)

## ---- echo = FALSE-------------------------------------------------------
consumer1_models <- multiModelTP(consumer1, n.adapt = 500, n.iter = 500, burnin = 500)
consumer1_models <- tRophicPosition:::Bilagay_models$consumer1_models

## ------------------------------------------------------------------------
consumer2 <- generateTPData(dCb1 = -17.3, dNb1 = 14.2,
                            dCc = -15, dNc = 21,
                            dCb2 = -12.7, dNb2 = 15.4,
                            consumer = "Consumer 2")

consumer2$deltaN <- TDF(author = "McCutchan", element = "N")
consumer2$deltaC <- TDF(author = "McCutchan", element = "C")

## ---- eval = FALSE-------------------------------------------------------
#  consumer2_models <- multiModelTP(consumer2)

## ---- eval = FALSE, echo = FALSE-----------------------------------------
#  consumer2_models <- multiModelTP(consumer2)
#  Bilagay_models <- tRophicPosition:::Bilagay_models
#  Bilagay_models$consumer2_models <- consumer2_models
#  Bilagay_models$consumer2_models$samples <- NULL
#  devtools::use_data(Bilagay_models, internal = TRUE, overwrite = TRUE)

## ---- echo = FALSE-------------------------------------------------------
consumer2_models <- multiModelTP(consumer2, n.adapt = 500, n.iter = 500, burnin = 500)
consumer2_models <- tRophicPosition:::Bilagay_models$consumer2_models

## ---- eval = FALSE-------------------------------------------------------
#  str(consumer1_models)

## ---- fig.width = 6, fig.height = 4--------------------------------------
# For consumer 1 (based on Post's (2002) TDF values)
credibilityIntervals(consumer1_models$gg, x = "model")

## ---- fig.width = 6, fig.height = 4--------------------------------------
# For consumer 2 (based on McCutchan's (2003) TDF values)
credibilityIntervals(consumer2_models$gg, x = "model")

## ------------------------------------------------------------------------
# Here we see that we have 4000 posterior samples of 3 parameters (one for each Bayesian model) for consumer1
str(consumer1_models$TP)

# And also 4000 posterior samples (for each 3 Bayesian models) for consumer2
str(consumer2_models$TP)

# But the names of each variables are the same for both consumers
# For consumer1
names(consumer1_models$TP)

# For consumer2
names(consumer2_models$TP)

# So, we change them in order to compare them.
# To make things clear, consumer 1 will be "Post" and consumer 2 will be "McCutchan". 
# Also, one baseline Bayesian model will be model1, two baselines model will be model2
# and two baselines full model will be model2F
names(consumer1_models$TP) <- c("Post-model1", "Post-model2", "Post-model2F")
names(consumer2_models$TP) <- c("McCutchan-model1", "McCutchan-model2", "McCutchan-model2F")

## ------------------------------------------------------------------------
# Here we combine posterior estimates of trophic position for both consumers
combined_models <- c(consumer1_models$TP, consumer2_models$TP)

# Then we calculate a summary of posterior trophic position
sapply(combined_models, summary)

# And we calculate the modes
getPosteriorMode(combined_models)

## ------------------------------------------------------------------------
compareTwoDistributions(combined_models$"Post-model1", combined_models$"McCutchan-model1", test = "<=")

## ------------------------------------------------------------------------
pairwiseComparisons(combined_models, test = "<=")

## ------------------------------------------------------------------------
# First we  combine both consumers isotope values into a named list
consumers <- list("consumer1" = consumer1, "consumer2" = consumer2)

# And then, we calculate parametric TP using a loop for
for (consumer in consumers) parametricTP(consumer)

