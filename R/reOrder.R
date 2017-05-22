#' Internal function that re-order variables from a data frame.
#'
#' Not intended to be used by the user.
#'
#' @param isotopeData data frame containing the data to be ordered.
#'
reOrder <- function (isotopeData = NULL){

  df <- data.frame(d13C = isotopeData$dCc,
                   d15N =isotopeData$dNc,
                   Factor = rep("Secondary consumer",
                                length(isotopeData$dCc)))

  df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                              d15N = isotopeData$dNb1,
                              Factor = rep("Baseline 1",
                                           length(isotopeData$dCb1))))

  df <- rbind (df, data.frame(d13C = isotopeData$dCb2,
                              d15N = isotopeData$dNb2,
                              Factor = rep("Baseline 2",
                                           length(isotopeData$dCb2))))


}
