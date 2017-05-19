#' Multiple models calculation of trophic position
#'
#'
#' @param siData
#' @param lambda
#' @param n.chains
#' @param n.adapt
#' @param n.iter
#' @param burnin
#' @param thin
#' @param models
#' @param print
#' @param quiet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

multiModelTP <- function (siData = siData, lambda = 2,
                          n.chains = 2,
                          n.adapt = 20000,
                          n.iter = 20000,
                          burnin = 20000,
                          thin = 10,
                          models = c("oneBaseline",
                                     "twoBaselines",
                                     "twoBaselinesFull"),
                          print = FALSE,
                          quiet = FALSE,
                          ...)
  {

  #To DO
  dots <- list(...)

  parallel <- NULL

  if (class(siData) != "isotopeData") {
    if (checkNames(df = siData, flag = 4)) class(siData) <- "isotopeData"
    else stop("We need an isotopeData class object")
    }


  HPDs  <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(HPDs) <- c("model", "community", "species",
                      "lower", "upper", "median", "mode", "alpha.lower",
                      "alpha.upper", "alpha.median", "alpha.mode")

  easyTP_list <- list()
  easyAlpha_list <- list()
  easySamples_list <- list()

  for (model in models) {

    if (isTRUE(print)) message(paste("################### Model: ", model))

    siData_mod <- siData
    # Check this...
    # variable.names = c("TP", "alpha", "muDeltaC")
    variable.names = c("TP", "alpha")


    if (model == "oneBaseline") {
      model.string <- tRophicPosition::jagsOneBaseline(lambda = lambda)
      model_txt <- "1b"
      myvars <- names(siData) %in% c("dCb1", "dNb2", "dCb2", "dCc", "deltaC")
      siData_mod <- siData[!myvars]
      variable.names = c("TP", "muDeltaN")
      }

    else if (model == "twoBaselines") {
      model.string <- tRophicPosition::jagsTwoBaselines(lambda = lambda)
      model_txt <- "2b"
      myvars <- names(siData) %in% c("deltaC")
      siData_mod <- siData[!myvars] }

    else if (model == "twoBaselinesFull"){
      model.string <- tRophicPosition::jagsTwoBaselinesFull(lambda = lambda)
      model_txt <- "2bf"
      }

    if (!is.null(parallel) & is.numeric(parallel)) {
      #TO DO...

      class(siData_mod) <- "list"

      if (isTRUE(print)) {
        summarise = TRUE
        plots = TRUE
      }
      else {
        summarise = FALSE
        plots = FALSE
      }

      runJagsOut <- runjags::run.jags(method = "parallel", model = model,
                                      monitor = c("TP", "muDeltaN"),
                                      data = siData_mod, n.chains = n.chains,
                                      adapt = n.adapt, burnin = burnin,
                                      sample = n.iter, thin = 5,
                                      summarise = summarise, plots = plots)

      if (isTRUE(print)) {
        plot(runJagsOut)
        print(runJagsOut)
      }

      easySamples_list[[model]] <- runJagsOut

      break

      }


    else {

      modelTP <- TPmodel(data = siData_mod,
                         model.string = model.string,
                         n.chains = n.chains,
                         n.adapt = n.adapt, quiet = quiet, ...)

      samples <- posteriorTP(modelTP,
                             variable.names = variable.names,
                             n.iter = n.iter + burnin,
                             thin = thin, quiet = quiet, ...)

      samples <- window(samples, start = n.adapt + burnin,
                        end = n.adapt + burnin + n.iter)

      }

    if (isTRUE(print)) {

      if (!is.null(attributes(siData)$community) & !is.null(attributes(siData)$consumer))

        plot(samples, sub = paste(model,
                                  attributes(siData)$community,
                                  attributes(siData)$consumer))

      else

          if(!is.null(attributes(siData)$consumer))

            plot(samples, sub = paste(model,
                                      attributes(siData)$consumer))

          else
            plot(samples, sub = model)

      print(summary(samples))
      print(coda::gelman.diag(samples))

      }

    TP.combined <- coda::mcmc(do.call(rbind, samples))
    HPD <- coda::HPDinterval(TP.combined)
    lower <- HPD[1]
    upper <- HPD[3]
    median <- median(TP.combined[,1])
    mode <- hdrcde::hdr(TP.combined[,1])$mode

    if (model == "oneBaseline") alpha.lower <- NA
    else alpha.lower <- HPD[2]

    if (model == "oneBaseline") alpha.upper <- NA
    else alpha.upper <- HPD[4]

    if (model == "oneBaseline") {
      alpha.median <- NA
      alpha.mode <- NA

      } else {
        alpha.median <- median(TP.combined[,2])
        alpha.mode <- hdrcde::hdr(TP.combined[,2])$mode
        }

    if (!is.null(attributes(siData)$community)) community =  attributes(siData)$community
    else community = NA

    if (!is.null(attributes(siData)$consumer)) species = attributes(siData)$consumer
    else species = NA

    df <- data.frame("model" = model_txt,
                     "community" = community,
                     # "consumer" = consumer,
                     "species" = species,
                     "lower" = lower,
                     "upper" = upper,
                     "median" = median,
                     "mode" = mode,
                     "alpha.lower" = alpha.lower,
                     "alpha.upper" = alpha.upper,
                     "alpha.median" = alpha.median,
                     "alpha.mode" = alpha.mode)

    HPDs <- rbind(HPDs, df)

    easyTP_list[[model_txt]] <- as.matrix(samples)[,1]
    easyAlpha_list[[model_txt]] <- as.matrix(samples)[,2]
    easySamples_list[[model_txt]] <- samples
  }

  if (model != "oneBaseline")
    return(list("TP" = as.list(easyTP_list),
                "alpha" = as.list(easyAlpha_list),
                "gg" = HPDs,
                "samples" = easySamples_list))
  else
    return(list("TP" = as.list(easyTP_list),
                "muDeltaN" = as.list(easyAlpha_list),
                "gg" = HPDs,
                "samples" = easySamples_list))

  # list("TP" = easyTP_list,
  #      "alpha" = easyAlpha_list,
  #      "gg" = HPDs,
  #      "samples" = easySamples_list)


}
