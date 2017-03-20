#' Internal function that stack variables from a named list into a data frame.
#'
#' Not intended to be used by the user.
#'
#' @param isotopeData a named list composed at least of 6 vectors, dNb1, dCb1,
#' dCc, dNc, deltaN and deltaC
#' @param baselines integer stating the number of baselines.
#' @param consumer string indicating the name of the consumer. By default it is
#' Consumer
#' @param b1 string indicating the baseline 1. By default is pelagic baseline.
#' @param b2 string indicating the baseline 2. By default is benthic baseline.
#'
#' @return a dataframe with 3 columns: d13C, d15N and Factor. Factor is a factor
#' with "Consumer", "Pelagic baseline" and "Benthic baseline" as levels.
#' @export
#'
#' @examples

toStacked <- function (isotopeData = NULL, baselines = 1, consumer = consumer,
                       b1 = b1, b2 = b2){

  if(!is.null(isotopeData)){

    if (baselines == 1) {

      if (checkNames(isotopeData, 1)) {

        df <- data.frame(d13C = isotopeData$dCc,
                         d15N =isotopeData$dNc,
                         Factor = rep(consumer,
                                      length(isotopeData$dCc)))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                                    d15N = isotopeData$dNb1,
                                    Factor = rep(b1,
                                                 length(isotopeData$dCb1))))
        return(df)

      } else {stop("on checkNames(isotopeData, 1")}

    } else if (baselines == 2){

      if (checkNames(isotopeData, 2)) {

        df <- data.frame(d13C = isotopeData$dCc,
                         d15N =isotopeData$dNc,
                         Factor = rep(consumer,
                                      length(isotopeData$dCc)))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                                    d15N = isotopeData$dNb1,
                                    Factor = rep(b1,
                                                 length(isotopeData$dCb1))))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb2,
                                    d15N = isotopeData$dNb2,
                                    Factor = rep(b2,
                                                 length(isotopeData$dCb2))))
        return(df)

      } else {stop("on checkNames(isotopeData, 2")}

#      } else if (baselines == 3){
#
#       if (checkNames(isotopeData, 3)) {
#
#         df <- data.frame(d13C = isotopeData$dCc,
#                          d15N =isotopeData$dNc,
#                          Factor = rep(consumer,
#                                       length(isotopeData$dCc)))
#
#         df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
#                                     d15N = isotopeData$dNb1,
#                                     Factor = rep("Baseline 1",
#                                                  length(isotopeData$dCb1))))
#
#         df <- rbind (df, data.frame(d13C = isotopeData$dCb2,
#                                     d15N = isotopeData$dNb2,
#                                     Factor = rep("Baseline 2",
#                                                  length(isotopeData$dCb2))))
#
#         df <- rbind (df, data.frame(d13C = isotopeData$dCb3,
#                                     d15N = isotopeData$dNb3,
#                                     Factor = rep("Baseline 3",
#                                                  length(isotopeData$dCb3))))
#
#         return(df)
#
#       } else {stop("on checkNames(isotopeData, 3")}

    } else {
      stop("Baseline must be either 1 or 2. Check the argument 'baselines'.")
    }

  } else {
    stop("IsotopeData is NULL")
    }
}
