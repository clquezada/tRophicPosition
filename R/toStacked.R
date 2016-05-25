#' Internal function that stack variables from a named list into a data frame.
#'
#' Not intended to be used by the user.
#'
#' @param isotopeData a named list composed at least of 6 vectors, dNb1, dCb1,
#' dCsc, dNsc, deltaN and deltaC
#' @param baselines integer stating the number of baselines.
#'
#' @return
#' @export
#'
#' @examples

toStacked <- function (isotopeData = NULL, baselines = 1){

  if(!is.null(isotopeData)){

    if (baselines == 1) {

      if (checkNames(isotopeData, 1)) {

        df <- data.frame(d13C = isotopeData$dCsc,
                         d15N =isotopeData$dNsc,
                         Factor = rep("Secondary consumer",
                                      length(isotopeData$dCsc)))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                                    d15N = isotopeData$dNb1,
                                    Factor = rep("Baseline 1",
                                                 length(isotopeData$dCb1))))
        return(df)

      } else {stop("on checkNames(isotopeData, 1")}

    } else if (baselines == 2){

      if (checkNames(isotopeData, 2)) {

        df <- data.frame(d13C = isotopeData$dCsc,
                         d15N =isotopeData$dNsc,
                         Factor = rep("Secondary consumer",
                                      length(isotopeData$dCsc)))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                                    d15N = isotopeData$dNb1,
                                    Factor = rep("Baseline 1",
                                                 length(isotopeData$dCb1))))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb2,
                                    d15N = isotopeData$dNb2,
                                    Factor = rep("Baseline 2",
                                                 length(isotopeData$dCb2))))
        return(df)

      } else {stop("on checkNames(isotopeData, 2")}

    }  else if (baselines == 3){

      if (checkNames(isotopeData, 3)) {

        df <- data.frame(d13C = isotopeData$dCsc,
                         d15N =isotopeData$dNsc,
                         Factor = rep("Secondary consumer",
                                      length(isotopeData$dCsc)))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                                    d15N = isotopeData$dNb1,
                                    Factor = rep("Baseline 1",
                                                 length(isotopeData$dCb1))))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb2,
                                    d15N = isotopeData$dNb2,
                                    Factor = rep("Baseline 2",
                                                 length(isotopeData$dCb2))))

        df <- rbind (df, data.frame(d13C = isotopeData$dCb3,
                                    d15N = isotopeData$dNb3,
                                    Factor = rep("Baseline 3",
                                                 length(isotopeData$dCb3))))

        return(df)

      } else {stop("on checkNames(isotopeData, 3")}

    } else {
      stop("Baseline must be either 1, 2 or 3. Check the argument 'baselines'.")
    }

  } else {
    stop("IsotopeData is NULL")
    }
}
