#'A function to generate stable isotope data for analysis in R
#'
#'This function generates random stable isotope data (unlimited elements) to use
#'within the stable isotope R packages.
#'
#'@return Missing.
#'
#'@export
#'
#' @examples
#'

generateIsotopeData <- function (matrix = NULL,
                                 labels = NULL,
                                 iso_labels = NULL,
                                 seed = 3,
                                 ...) {
  set.seed(seed)

  matrix <- matrix(c(25, -3, 1, 5, 1,
                     25, -6, 1, 1, 1,
                     25, -5, 1, 2, 1),
                   ncol = 5, byrow = TRUE)

  if(is.null(matrix) |
     length(matrix) != length(unlist(matrix)) |
     sum(is.na(matrix))>0)
    stop("Check the matrix, it may cointain NULL or NA values")

  labels <- c("consumer1", "source1", "source2")
  if(is.null(labels) |
     length(labels) != length(unlist(labels)) |
     sum(is.na(labels))>0)
    stop("Check the labels, it may cointain NULL or NA values")

  if(nrow(matrix) != length(labels))
    stop("The length of labels must be equal as the number of rows in matrix")

  if(ncol(matrix) %% 2 == 0 | ncol(matrix) < 3)
    stop("We need a matrix with at least 3 columns with n, and mean and sd per
         isotope. Please check the matrix")

  iso_labels <- c("d13C", "d15N")
  if(!is.null(iso_labels))
    if((ncol(matrix)-1)/2 != length(iso_labels))
      stop("Please check the matrix, iso_labels should match the isotopes in
           the matrix")

  meanSD <- function(vct) {

    n <- vct[1]
    mean <- vct[2]
    sd <- vct[3]

    X <- stats::rnorm(n, mean, sd)
    MEAN <- mean
    SD <- sd
    Z <- (((X - mean(X, na.rm = TRUE))/sd(X, na.rm = TRUE))) * SD
    MEAN + Z

  }

  apply(matrix, 1, meanSD)[,1]

  df <- data.frame()
  df[["iso1"]] <- c(1,2,3,4,5)

  for (j in 1:nrow(matrix)) {
    n <- matrix[j,1]

  }

}
