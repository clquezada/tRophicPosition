#' Function to perform pairwise comparisons between two or more posterior
#' distributions
#'
#' Function to compare two or more posterior distributions and test a
#' hypothesis, in a Bayesian context
#'
#' @param df data frame with a collection of numerical values (posterior
#' samples) to be compared.
#' @param test string with the logical test to be used in comparisons. Can be <,
#' <=, > or >=.
#' @param print logical value to indicate whether the output should be printed
#' or not.
#'
#' @return a symmetrical matrix with probabilities given sum(dist1 >= dist2) /
#' length(dist1) for each comparison.
#' @export
#'
#' @examples
#' a <- rnorm(100, 2, 0.1)
#' b <- rnorm(100, 1.8, 0.1)
#' c <- rnorm(100, 2.2, 0.1)
#' pairwiseComparisons(list("a" = a, "b" = b, "c" = c))
#'
pairwiseComparisons <- function (df, test = "<=", print = FALSE) {

  pairwise <- matrix(nrow = length(names(df)), ncol = length(names(df)))

  rownames(pairwise) <- paste0("[",seq_along(names(df)),"]", " ", names(df))
  colnames(pairwise) <- paste0("[",seq_along(names(df)),"]")

  counter.i <- 1
  for (i in names(df)) {
    counter.j <- 1

    for (j in names(df)) {
      comparison <- tRophicPosition::compareTwoDistributions(df[[i]],
                                                             df[[j]],
                                                             test = test)

      #message("Comparison between ", names(df[i]), " and ", names(df[j]))
      #print(comparison)

      if (i == j) pairwise[counter.i, counter.j] <- 0
      else pairwise[counter.i, counter.j] <- round(comparison,3)

      counter.j <- counter.j + 1

    }

    counter.i <- counter.i + 1
  }
  if (print) print(pairwise)
  pairwise

}
