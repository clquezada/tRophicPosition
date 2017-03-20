#' Function to perform a pairwise comparison between several posterior distributions
#'
#' @param df
#' @param print
#' @param test
#'
#' @return
#' @export
#'
#' @examples
#'
pairwiseComparisons <- function (df, test = "<=", print = FALSE) {

  pairwise <- matrix(nrow = length(names(df)), ncol = length(names(df)))

  rownames(pairwise) <- names(df)
  colnames(pairwise) <- names(df)

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
      else pairwise[counter.i, counter.j] <- comparison

      counter.j <- counter.j + 1

    }

    counter.i <- counter.i + 1
  }
  if (print) print(pairwise)
  pairwise

}
