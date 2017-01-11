#' Construct isotope data from a data frame
#'
#' @param df
#' @param d15N
#' @param d13C
#' @param consumer
#' @param grouping
#' @param b1
#' @param b2
#' @param TEF
#'
#' @return
#' @export
#'
#' @examples
isotopeData <- function (df, d15N = "d15N", d13C = "d13C", grouping = "FG",
                         consumer = NULL, b1 = NULL, b2 = NULL, TEF = NULL) {
  require(dplyr)

  if (class(df) != "data.frame") stop("You need to use a data frame with this function")

  #get the names from the user
  names <- c(d15N, d13C, grouping)

  if (!tRophicPosition::checkNames(df, names)) stop("Error on check names")
  if (is.null(b1)) stop("You have to indicate the baseline 1")
  if (is.null(b2)) stop("You have to indicate the baseline 2")


  # We need to separate the baselines
  # First we select d15N and d13C for baseline 1

  if (length(b1) == 1)  {
    dNb1 <- df[[d15N]][which(df[[grouping]] == b1)]
    dCb1 <- df[[d13C]][which(df[[grouping]] == b1)]
    } else {
      dNb1 <- numeric()
      for (group in b1) dNb1 <- c(dNb1, df[[d15N]][which(df[[grouping]] == group)])
      dCb1 <- numeric()
      for (group in b1) dCb1 <- c(dCb1, df[[d13C]][which(df[[grouping]] == group)])
    }

  # Then we select d15N and d13C for baseline 2
  if (length(b2) == 1)  {
    dNb2 <- df[[d15N]][which(df[[grouping]] == b2)]
    dCb2 <- df[[d13C]][which(df[[grouping]] == b2)]
  } else {
    dNb2 <- numeric()
    for (group in b2) dNb2 <- c(dNb2, df[[d15N]][which(df[[grouping]] == group)])
    dCb2 <- numeric()
    for (group in b2) dCb2 <- c(dCb2, df[[d13C]][which(df[[grouping]] == group)])
  }

  #Finally we select d15N and d13C for the consumer
  dNc <- df[[d15N]][which(df[[grouping]] == consumer)]
  dCc <- df[[d13C]][which(df[[grouping]] == consumer)]

  if (is.null(TEF)) TEF <- tRophicPosition::TEF()

  if ((class(TEF) == "numeric")){

    isotopeList <- list("dNb1" = dNb1,
                        "dCb1" = dCb1,
                        "dNb2" = dNb2,
                        "dCb2" = dCb2,
                        "dNc" = dNc,
                        "dCc" = dCc,
                        "deltaN" = TEF)

  } else {

    if (tRophicPosition::checkNames(TEF, flag = c("deltaN", "deltaC")))

      isotopeList <- list("dNb1" = dNb1,
                          "dCb1" = dCb1,
                          "dNb2" = dNb2,
                          "dCb2" = dCb2,
                          "dNc" = dNc,
                          "dCc" = dCc,
                          "deltaN" = TEF$deltaN,
                          "deltaC" = TEF$deltaC)
  }

  class(isotopeList) <- "isotopeData"

  isotopeList

}
