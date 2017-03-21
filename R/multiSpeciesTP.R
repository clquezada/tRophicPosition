#' Multiple species calculation of trophic position
#'

#' @param siDataList
#' @param lambda
#' @param n.chains
#' @param n.adapt
#' @param n.iter
#' @param burnin
#' @param thin
#' @param model
#' @param print
#' @param parallel
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

multiSpeciesTP <- function (siDataList = siDataList, lambda = 2,
                            n.chains = 2,
                            n.adapt = 20000,
                            n.iter = 20000,
                            burnin = 20000,
                            thin = 10,
                            model = "oneBaseline",
                            print = FALSE,
                            parallel = NULL, ...)
  {

  #To DO
  # dots <- list(...)
  # if (!is.null(dots)) cat(names(...))

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
                               print = print)

    if (is.null(attributes(siDataList[[i]])$consumer)) TP_results$gg$species <- names(siDataList[i])

    #TP_results$gg$combined <- names(siDataList[i])

    multiSpeciesTP_list[[names(siDataList[i])]] <- TP_results

    names(TP_results[["TP"]]) <- paste(names(siDataList[i]), names(TP_results[["TP"]]))
    if (ncol(multiSpecies_TP) == 0)
      multiSpecies_TP <- as.data.frame(TP_results[["TP"]])
    else
      multiSpecies_TP <- cbind(multiSpecies_TP, as.data.frame(TP_results[["TP"]]))

    names(TP_results[["alpha"]]) <- paste(names(siDataList[i]), names(TP_results[["alpha"]]))
    if (ncol(multiSpecies_alpha) == 0)
      multiSpecies_alpha <- as.data.frame(TP_results[["alpha"]])
    else
      multiSpecies_alpha <- cbind(multiSpecies_alpha, as.data.frame(TP_results[["alpha"]]))

    #multiSpecies_TP[[names(siDataList[i])]] <- TP_results[["TP"]]
    #multiSpecies_alpha[[names(siDataList[i])]] <- TP_results[["alpha"]]
    multiSpeciesTP_df <- rbind(multiSpeciesTP_df, TP_results$gg)

  }

  multiSpeciesTP_df$combined <- paste(multiSpeciesTP_df$community,
                              multiSpeciesTP_df$species,
                              multiSpeciesTP_df$model, sep = "-")


  object <- list("multiSpeciesTP" = multiSpeciesTP_list,
                 "df" = multiSpeciesTP_df,
                 "TP's" = multiSpecies_TP,
                 "Alpha's" = multiSpecies_alpha)

  class(object[["TP's"]]) <- "posteriorTP"
  class(object[["Alpha's"]]) <- "posteriorAlpha"

  return(object)

}
