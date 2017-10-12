#' Multiple species calculation of trophic position
#'
#' This function takes a named list of isotopeData class objects and calculates one
#' or more Bayesian models of trophic position for each element of the list.
#'
#' @param siDataList a named list of isotopeData class objects.
#' @param lambda numerical value, represents the trophic level for baseline(s).
#' @param n.chains number of parallel chains for the model. If convergence
#'   diagnostics (such as Gelman-Rubin) are printed, n.chains needs to be >= 2.
#' @param n.adapt number of adaptive iterations, before the actual sampling.
#' @param n.iter number of iterations for Bayesian modelling (posterior
#'   sampling).
#' @param burnin number of iterations discarded as burn in.
#' @param thin thinning. Number of samples discarded while performing posterior
#'   sampling.
#' @param model string or list representing Bayesian models. At the moment they
#'   can be "oneBaseline", "twoBaselines" and/or "twoBaselinesFull".
#' @param print logical value to indicate whether Gelman and Rubin's convergence
#'   diagnostic and summary of samples are printed.
#' @param quiet logical value to indicate whether messages generated during
#'   compilation will be suppressed, as well as the progress bar during
#'   adaptation.
#' @param ... additional arguments passed to this function.
#'
#' @return A list of 4 elements. The output is organised as lists nested. The
#'   first element (multiSpeciesTP) has the gg data frame returned by
#'   multiModelTP, the second element (df) is a data frame with summary
#'   information for all consumers and models, the third element (TPs) has the
#'   raw posterior trophic position for all consumers and models, and the last
#'   element (Alphas) has raw posterior of muDeltaN (if one baseline model was
#'   chosen) or alpha (if a two baselines model was chosen) for all consumers
#'   and models.
#' @export
#'
#' @examples
#'siDataList <- list("consumer1" = generateTPData(consumer = "consumer1"),
#'"consumer2" = generateTPData(consumer = "consumer2"))
#'models <- multiSpeciesTP(siDataList, model = "twoBaselines", n.adapt = 500,
#'n.iter = 500, burnin = 500)
#'credibilityIntervals(models$df, x = "species")
#'

multiSpeciesTP <- function (siDataList = siDataList, lambda = 2,
                            n.chains = 2,
                            n.adapt = 20000,
                            n.iter = 20000,
                            burnin = 20000,
                            thin = 10,
                            model = "oneBaseline",
                            print = FALSE,
                            quiet = FALSE,
                            ...)
  {

  #To DO
  # dots <- list(...)
  # if (!is.null(dots)) cat(names(...))
  parallel <- NULL

  multiSpeciesTP_list <- list()
  multiSpeciesTP_df <- data.frame()

  multiSpecies_TP <- data.frame()
  multiSpecies_alpha <- data.frame()

  species <- names(siDataList)

  for (i in seq_along(species)) {

    if (isTRUE(print)) message(paste("################### Species: ", names(siDataList[i])))

    TP_results <- multiModelTP(siData = siDataList[[i]],
                               lambda = lambda,
                               n.adapt = n.adapt,
                               n.iter = n.iter,
                               n.chains = n.chains,
                               burnin = burnin,
                               thin = thin,
                               models = model,
                               parallel = parallel,
                               print = print,
                               quiet = quiet)

    if (is.null(attributes(siDataList[[i]])$consumer)) TP_results$gg$species <- names(siDataList[i])

    #TP_results$gg$combined <- names(siDataList[i])

    multiSpeciesTP_list[[names(siDataList[i])]] <- TP_results

    names(TP_results[["TP"]]) <- paste(names(siDataList[i]), names(TP_results[["TP"]]))
    if (ncol(multiSpecies_TP) == 0)
      multiSpecies_TP <- as.data.frame(TP_results[["TP"]])
    else
      multiSpecies_TP <- cbind(multiSpecies_TP, as.data.frame(TP_results[["TP"]]))

    if (model != "oneBaseline"){

      names(TP_results[["alpha"]]) <- paste(names(siDataList[i]), names(TP_results[["alpha"]]))

      if (ncol(multiSpecies_alpha) == 0)
        multiSpecies_alpha <- as.data.frame(TP_results[["alpha"]])
      else
        multiSpecies_alpha <- cbind(multiSpecies_alpha, as.data.frame(TP_results[["alpha"]]))
    }

    #multiSpecies_TP[[names(siDataList[i])]] <- TP_results[["TP"]]
    #multiSpecies_alpha[[names(siDataList[i])]] <- TP_results[["alpha"]]
    multiSpeciesTP_df <- rbind(multiSpeciesTP_df, TP_results$gg)

  }

  multiSpeciesTP_df$combined <- paste(multiSpeciesTP_df$community,
                              multiSpeciesTP_df$species,
                              multiSpeciesTP_df$model, sep = "-")

  if (model != "oneBaseline") {
    object <- list("multiSpeciesTP" = multiSpeciesTP_list,
                   "df" = multiSpeciesTP_df,
                   "TPs" = multiSpecies_TP,
                   "Alphas" = multiSpecies_alpha)

    class(object[["Alphas"]]) <- "posteriorAlpha"

  } else {
    object <- list("multiSpeciesTP" = multiSpeciesTP_list,
                   "df" = multiSpeciesTP_df,
                   "TPs" = multiSpecies_TP)
    }

  class(object[["TPs"]]) <- "posteriorTP"

  return(object)

}
