#' Function that summarise a data frame containing a stable isotope values (d13C
#' and d15N) grouping by Species and FG columns
#'
#' A wrapper of plyr:ddply to summarise a data frame.
#'
#' @param df a data frame that contains the isotope values. It needs to have the
#'  following columns: d13C, d15N, Species and FG. Species stands for the
#'  scientific name (or common name), and FG stands for the functional group for
#'  each species. If the data frame do not have Species and FG columns, it will
#'  raise an error. If the columns change their names, they need to be stated
#'  as well in the grouping variable.
#' @param grouping a vector with the name of the columns (variables) that will
#' be used to summarize, and plot the data frame.
#' @param ... optional arguments that are passed to the function for later use.
#' @param printSummary a boolean flag indicating whether the summary is printed.
#'
#' @return a data frame with the summary of the data frame.
#'
#' @export
#'
#' @examples

summariseIsotopeData <- function (df = NULL, grouping = c("Species", "FG"),
                                  printSummary = FALSE, ...){

  if (is.null(tRophicPosition::checkNames(df, c("d13C", "d15N", grouping))))
    stop("Check the grouping variable or the names in your dataframe")

  summary <- plyr::ddply(df, grouping, plyr::summarise,
                         NC = length(d13C),
                         meanC = mean(d13C),
                         sdC = sd(d13C),
                         NN = length(d15N),
                         meanN = mean(d15N),
                         sdN = sd(d15N))

  if (printSummary)  print(summary)

  summary

}
