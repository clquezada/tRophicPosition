
#' Function to compare two TP posterior distributions and test an hypothesis,
#' in a Bayesian context
#'
#' @param dist1 A collection of TP data (posterior distribution)
#' @param dist2 A collection of TP data (posterior distribution)
#' @param test A logical operator which states what to test for
#'
#' @return
#' @export
#'
#' @examples
#'
compareTwoDistributions <- function (dist1 = NULL,
                                     dist2 = NULL,
                                     test = "<=") {

  if (is.null(dist1) | is.null(dist2)) {

    warning("At least one of the distributions you want to compare is NULL.")
    return(NULL)
  }

  if (length(dist1) != length(dist2)) {

    warning("You are comparing two distributions that have different length.
            Although this is possible, it is likely that the two distributions
            came from different analysis, thus you need to check if the
            distributions you are comparing are those you actually want to
            compare. The analysis will stop now.")

    cat("Length of distribution 1:", length(dist1), sep=" ")
    cat("Length of distribution 2:", length(dist2), sep=" ")

    return(NULL)
  }

  if (test == "<=") {
    return(sum(dist1 <= dist2) / length(dist1))
  }

  if (test == "<") {
    return(sum(dist1 < dist2) / length(dist1))
  }

  if (test == ">") {
    return(sum(dist1 > dist2) / length(dist1))
  }

  if (test == ">=") {
    return(sum(dist1 >= dist2) / length(dist1))
  }

  warning('Do you check that the logical operator is "<", "<=", ">"" or ">="?
          Otherwise the function will not work properly.')
}
