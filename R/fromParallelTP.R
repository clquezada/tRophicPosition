#' Function to extract raw data from parallel calculations of trophic position
#'
#' This function parses a data frame where it was stored the data from parallel
#' calculations using multiModelTP and a list of isotopeData class objects.
#'
#' @param df data frame, variable where it was saved the output from parallel
#' calculations of TP
#' @param get string, could be either "TP", "alpha" or "summary." In case of TP
#' this function will extract the trophic position data, in case of alpha, this
#' function will extract the alpha parameter data, and in case of summary, it
#' will return a data frame ready to plot with the function credibilityIntervals.
#'
#' @return when selecting "TP" or "alpha", this function returns a posteriorTP
#' or a posteriorAlpha object with the data. When selecting "summary", this
#' function returns a data frame ready to be used with credibilityIntervals().
#' @export
#'
#' @examples
#' \dontrun{
#' data("Bilagay")
#' BilagayList <- extractIsotopeData(Bilagay,
#' communityColumn = "Location",speciesColumn = "FG",
#' b1 = "Pelagic_BL", b2 = "Benthic_BL",
#' baselineColumn = "FG",
#' deltaC = TDF(author = "McCutchan", element = "C", type = "muscle"),
#' deltaN = TDF(author = "McCutchan", element = "N", type = "muscle"))
#' Bilagay_TPmodels <- parallel::parLapply(cluster,
#' BilagayList, multiModelTP, adapt = 20000,
#' n.iter = 20000, burnin = 20000, n.chains = 5,
#' model = "twoBaselinesFull")
#' ggplot_df <- fromParallelTP(Bilagay_TPmodels, get = "summary")
#' credibilityIntervals(ggplot_df, x = "community",
#' xlab = "Location along N-S gradient")
#' }
#'
fromParallelTP <- function (df = NULL, get = NULL) {

  extract <- function(df, object) {

    returned_obj <- list()

    if (length(seq_along(df[1])) == 1)
      for (species in seq_along(df))
        returned_obj[[names(df[species])]] <- df[[species]][[object]][[1]]

      else {
        for (species in seq_along(df))
          for (model in seq_along(species))
            returned_obj[[paste0(names(df[species]), "-",
                                 names(df[[species]][[object]][model]))]] <-
              df[[species]][[object]][[model]]
      }

      if (get == "alpha") class(returned_obj) <- "posteriorAlpha"

      if (get == "TP") class(returned_obj) <- "posteriorTP"

      return(returned_obj)

  }

  if (get == "summary") {

    returned_obj <- data.frame()

    for (species in df)
      returned_obj <- rbind(returned_obj, species$gg)

    return(returned_obj)

  } else return(extract(df, get))

}
