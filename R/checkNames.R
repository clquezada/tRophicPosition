#' Internal function that check the names of a list.
#'
#' Not intended to be used by the user.
#'
#' @param df named list with the variables to be checked for.
#' @param baselines integer stating the number of baselines.
#'
#' @return
#' @export
#'
#' @examples
#'
checkNames <- function (df = NULL, baselines = NULL) {

  if (baselines == 1) {
    namesDF <- c("dNb1", "dCb1", "dNsc", "dCsc")

  } else if (baselines == 2) {
    namesDF <- c("dNb1", "dNb2", "dCb1", "dCb2", "dNsc", "dCsc")

  } else if (baselines == 3) {
    namesDF <- c("dNb1", "dNb2", "dNb3", "dCb1", "dCb2", "dCb3", "dNsc",
                 "dCsc")

  } else {

    stop("Baseline must be either 1, 2 or 3. Check the argument 'baselines'.")
    }

  for (name in namesDF) {
    counter <- 0

    if ((name %in% names(df)) == FALSE) {
      counter <- counter + 1
    }
  }

  if (counter > 0) {
    cat("Names of your dataframe: ", names(df), "\n")
    cat("Names expected: ", namesDF, "\n")
    cat("Number of baselines: ", baselines, "\n")
    message("
            You have at least one variable in your dataframe that does not
            match the names expected for the number of baselines.")
    return(NULL)
  }

  return(TRUE)
}

