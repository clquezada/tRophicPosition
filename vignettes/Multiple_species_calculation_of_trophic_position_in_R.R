## ----eval = FALSE--------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)

## ----eval = FALSE--------------------------------------------------------
#  install_github("clquezada/tRophicPosition")

## ------------------------------------------------------------------------
library(tRophicPosition)

## ---- echo = FALSE, results = 'asis'-------------------------------------
data("Bilagay")
knitr::kable(Bilagay[c(1:3,130:133,660:661),], caption = "Structure of the file Bilagay_for_tRophicPosition.csv")

## ------------------------------------------------------------------------
system.file("extdata", "Bilagay_for_tRophicPosition.csv", package = "tRophicPosition")

## ----eval = FALSE--------------------------------------------------------
#  data("Bilagay")

## ----message = FALSE-----------------------------------------------------
# install.packages(dplyr) if you haven't installed it before
library(dplyr)
Bilagay <- Bilagay %>% mutate(Community = paste(Study,"-", Location, sep = ""))

## ------------------------------------------------------------------------
Bilagay <- Bilagay %>% arrange(NS)

## ------------------------------------------------------------------------
BilagayList <- extractIsotopeData(Bilagay, b1 = "Pelagic_BL", b2 = "Benthic_BL",
                                  baselineColumn = "FG", speciesColumn = "Spp",
                                  communityColumn = "Community",
                                  d13C = "d13C", d15N = "d15N")

## ----eval = FALSE--------------------------------------------------------
#  str(BilagayList)

## ----echo = FALSE--------------------------------------------------------
str(BilagayList[1:1])

## ----eval = FALSE--------------------------------------------------------
#  for (community in BilagayList) {
#    print(summary(community))
#    plot(community)
#    }

## ----echo = FALSE, fig.width =  7, fig.height = 5------------------------
for (community in BilagayList[1]) {
  print(summary(community))
  plot(community)
  }

## ---- eval = FALSE-------------------------------------------------------
#  Bilagay_models <- multiSpeciesTP(BilagayList, model = "twoBaselinesFull",
#                                               n.adapt = 10000, n.iter = 10000,
#                                               burnin = 10000, n.chains = 2, print = FALSE)

## ---- echo=FALSE---------------------------------------------------------
a <- multiSpeciesTP(BilagayList[1], model = "twoBaselinesFull",
                    n.adapt = 100, n.iter = 100,
                    burnin = 100, n.chains = 2, print = FALSE)

## ----eval = FALSE--------------------------------------------------------
#  # By default the mode is used in both trophic position and alpha plots
#  credibilityIntervals(Bilagay_models$df, x = "community", xlab ="Community")
#  
#  # If you want to use the median instead of the mode,
#  # just add y1 and y2 as arguments
#  credibilityIntervals(Bilagay_models$df, x = "community", xlab ="Community",
#                       y1 = "median", y2 = "alpha.median")

## ----echo = FALSE, fig.width = 7, fig.height = 5-------------------------
credibilityIntervals(tRophicPosition:::Bilagay_models$df, x = "community", xlab ="Community")

## ----eval = FALSE--------------------------------------------------------
#  # To get a numerical summary
#  sapply(Bilagay_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)
#  
#  # To get the mode
#  getPosteriorMode(Bilagay_models$"TPs")

## ----echo = FALSE--------------------------------------------------------
# To get a numerical summary
sapply(tRophicPosition:::Bilagay_models$"TPs", quantile, probs = c(0.025, 0.5, 0.975)) %>% round(3)

# To get the mode
getPosteriorMode(tRophicPosition:::Bilagay_models$"TPs")

## ----eval = FALSE--------------------------------------------------------
#  # First, we compare bilagay posterior trophic position estimates
#  pairwiseTP <- pairwiseComparisons(Bilagay_models$TPs[1:8], print = TRUE)
#  
#  # And then, we compare their posterior alpha estimates
#  pairwiseAlpha <- pairwiseComparisons(Bilagay_models$Alphas[1:8], print = TRUE)

## ----echo = FALSE--------------------------------------------------------
# First, we compare bilagay posterior trophic position estimates
pairwiseTP <- pairwiseComparisons(tRophicPosition:::Bilagay_models$TPs[1:8], print = TRUE)

# And then, we compare their posterior alpha estimates
pairwiseAlpha <- pairwiseComparisons(tRophicPosition:::Bilagay_models$Alphas[1:8], print = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  cl <- parallel::makePSOCKcluster(parallel::detectCores())

## ----eval = FALSE--------------------------------------------------------
#  # First we calculate the time elapsed with system.time() with parallel
#  time_parallel <- system.time(a <- parallel::parLapply(cl, BilagayList, multiModelTP,
#                                                 adapt = 500, n.iter = 500,
#                                                 burnin = 500))
#  
#  # Then we calculate the elapsed time with the normal version
#  time_serial <- system.time(b <- lapply(BilagayList, multiModelTP, quiet = TRUE,
#                                                 adapt = 500, n.iter = 500,
#                                                 burnin = 500))
#  
#  # We have to stop the cluster after each set of calculations
#  parallel::stopCluster(cl)
#  
#  # And print the elapsed time
#  print(rbind(time_parallel, time_serial))

## ----echo = FALSE--------------------------------------------------------
print(tRophicPosition:::Bilagay_models$time)

