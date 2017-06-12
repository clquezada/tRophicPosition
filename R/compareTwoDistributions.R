#' Function to compare two distributions and test a hypothesis, in a Bayesian
#' context
#'
#' @param dist1 A collection of numerical values (posterior distribution).
#' @param dist2 A collection of numerical values (posterior distribution).
#' @param test A logical operator which states what to test for. Might be "<",
#' "<=", ">" or ">=".
#' @param sample If sample is numeric, it will take 'sample' elements of each of
#' the distributions.
#' @param round integer to indicate number of decimals kept.
#' @param ... extra arguments are passed to compareTwoDistributions().
#'
#' @return probability given sum(dist1 "test" dist2) / length(dist1)
#' @export
#'
#' @examples
#'a <- rnorm(100, 2, 0.1)
#'b <- rnorm(100, 1.8, 0.1)
#'compareTwoDistributions(a, b)
#'
compareTwoDistributions <- function (dist1 = NULL,
                                     dist2 = NULL,
                                     test = "<=",
                                     sample = NULL,
                                     round = 3,
                                     ...) {

  # To do
  # implement ordered = TRUE within arguments
  #
  # sum_over_total <- function(dist1, dist2, test, round)
  #   round((sum(dist1 <= dist2) / length(dist1)), 3)

  if (is.null(dist1) | is.null(dist2)) {

    warning("At least one of the distributions you want to compare is NULL.")
    return(NULL)
  }

  if (length(dist1) != length(dist2)) {

    warning("
You are comparing two distributions that have different length.
Although this is possible, it is likely that the two distributions
came from different analysis, thus you need to check if the
distributions you are comparing are those you actually want to
            compare. The analysis will stop now.")

    cat("Length of distribution 1:", length(dist1), sep=" ")
    cat("Length of distribution 2:", length(dist2), sep=" ")

    return(NULL)
  }

  if (is.numeric(sample)) {

    dist1 <- base::sample(dist1, sample, ...)
    dist2 <- base::sample(dist2, sample, ...)

  }

  # if (class(dist1) == "")

  if (test == "<=") {
    return(round(sum(dist1 <= dist2) / length(dist1), round))
  }

  if (test == "<") {
    return(round(sum(dist1 < dist2) / length(dist1), round))
  }

  if (test == ">") {
    return(round(sum(dist1 > dist2) / length(dist1), round))
  }

  if (test == ">=") {
    return(round(sum(dist1 >= dist2) / length(dist1), round))
  }

  warning('
Have you checked that the logical operator is "<", "<=", ">"" or ">="?
          Otherwise the function will not work properly.')
}
