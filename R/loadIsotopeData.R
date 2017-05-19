#' Extract and load stable isotope data for one consumer from a data frame
#'
#'
#' @param df
#' @param consumer
#' @param b1
#' @param b2
#' @param groupingColumn
#' @param d13C
#' @param d15N
#' @param deltaC
#' @param deltaN
#' @param seed
#'
#' @return
#' @export
#'
#' @examples

loadIsotopeData <- function(df = NULL,
                            consumer = NULL,
                            b1 = "Baseline 1", b2 = NULL,
                            groupingColumn = "FG",
                            d13C = "d13C", d15N = "d15N",
                            deltaC = NULL, deltaN = NULL,
                            seed = 666) {

  # Usar consumer

tRophicPosition::extractIsotopeData(df, b1 = b1, b2 = b2, baselineColumn = groupingColumn,
                                    speciesColumn = groupingColumn, communityColumn = NULL,
                                    deltaC = deltaC, deltaN = deltaN,
                                    d13C = d13C, d15N = d15N,
                                    seed = seed)[[1]]
  }
