#' Construct isotope data from a data frame
#'
#' @param df
#' @param d15N
#' @param d13C
#' @param consumer
#' @param FG
#' @param b1
#' @param b2
#' @param TEF
#'
#' @return
#' @export
#'
#' @examples
isotopeData <- function (df, d15N = "d15N", d13C = "d13C", FG = "FG",
                         consumer = NULL, b1 = NULL, b2 = NULL, TEF = NULL) {

  if (class(df) != "data.frame") stop("You need to use a data frame with this function")

  #get the names from the user
  names <- c(d15N, d13C, FG)

  if (!tRophicPosition::checkNames(df, names)) stop("Error on check names")

  # We need to separate the baselines
  # First we select d15N and d13C for baseline 1
  dNb1 <- df[[d15N]][which(df[[FG]] == b1)]
  dCb1 <- df[[d13C]][which(df[[FG]] == b1)]

  # Then we select d15N and d13C for baseline 2
  dNb2 <- df[[d15N]][which(df[[FG]] == b2)]
  dCb2 <- df[[d13C]][which(df[[FG]] == b2)]

  #Finally we select d15N and d13C for the consumer
  dNc <- df[[d15N]][which(df[[FG]] == consumer)]
  dCc <- df[[d13C]][which(df[[FG]] == consumer)]

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
