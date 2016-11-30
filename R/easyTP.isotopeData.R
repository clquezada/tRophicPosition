#' Title
#'
#' @param siData
#' @param lambda
#' @param print
#'
#' @return
#' @export
#'
#' @examples
easyTP <- function (siData = siData, lambda = 2, print = TRUE,
                                n.adapt = 10000, n.iter = 10000, n.chains = 2,
                                models = c("oneBaseline", "twoBaselines", "twoBaselinesFull"))
  {

  if (class(siData) != "isotopeData") stop("We need an isotopeData class object")

  HPDs  <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(HPDs) <- c("model", "lower", "upper", "median", "alpha.lower",
                      "alpha.upper", "alpha.median")

  easyTP_list <- list()
  easyAlpha_list <- list()
  easySamples_list <- list()

  for (model in models) {

    if (isTRUE(print)) message(paste("################### Model: ", model))

    siData_mod <- siData

    if (model == "oneBaseline") {
      model.string <- tRophicPosition::jagsOneBaseline(lambda = lambda)
      myvars <- names(siData) %in% c("dCb1", "dNb2", "dCb2", "dCc", "deltaC")
      siData_mod <- siData[!myvars] }

    else if (model == "twoBaselines") {
      model.string <- tRophicPosition::jagsTwoBaselines(lambda = lambda)
      myvars <- names(siData) %in% c("deltaC")
      siData_mod <- siData[!myvars] }

    else if (model == "twoBaselinesFull")
      model.string <- tRophicPosition::jagsTwoBaselinesFull(lambda = lambda)

    modelTP <- TPmodel(data = siData_mod,
                     model.string = model.string,
                     n.chains = n.chains,
                     n.adapt = n.adapt)

    if (model == "oneBaseline")
      samples <- posteriorTP(modelTP,
                             variable.names = c("TP", "muDeltaN"),
                             n.iter = n.iter)
    else
      samples <- posteriorTP(modelTP,
                             variable.names = c("TP", "alpha"),
                             n.iter = n.iter)

    if (isTRUE(print)) plot(samples, sub = model)
    if (isTRUE(print)) print(summary(samples))
    if (isTRUE(print)) print(coda::gelman.diag(samples))

    TP.combined <- coda::mcmc(do.call(rbind, samples))
    HPD <- coda::HPDinterval(TP.combined)
    lower <- HPD[1]
    upper <- HPD[3]
    median <- median(TP.combined[,1])

    if (model == "oneBaseline") alpha.lower <- NA
    else alpha.lower <- HPD[2]

    if (model == "oneBaseline") alpha.upper <- NA
    else alpha.upper <- HPD[4]

    if (model == "oneBaseline") alpha.median <- NA
    else alpha.median <- median(TP.combined[,2])

    df <- data.frame("model" = model,
                     "lower" = lower,
                     "upper" = upper,
                     "median" = median,
                     "alpha.lower" = alpha.lower,
                     "alpha.upper" = alpha.upper,
                     "alpha.median" = alpha.median)

    HPDs <- rbind(HPDs, df)

    easyTP_list[[model]] <- as.matrix(samples)[,1]
    easyAlpha_list[[model]] <- as.matrix(samples)[,2]
    easySamples_list[[model]] <- samples
  }

  list("TP" = as.data.frame(easyTP_list),
       "alpha" = as.data.frame(easyAlpha_list),
       "gg" = HPDs,
       "samples" = easySamples_list)

}
