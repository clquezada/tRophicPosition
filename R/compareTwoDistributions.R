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
#'compareTwoDistributions(a, b, test = ">=")
#'compareTwoDistributions(a, b, test = "bhatt")
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
  #

  # #' Internal function that calculates the Bhattacharrya Coefficient.
  # #'
  # #' Not intended to be used by the user.
  bhatt.coeff <- function(x, y, bw = stats::bw.nrd0, ...) {
    ## Check data
    ## x and y
    check.class(x, "numeric")
    check.class(y, "numeric")

    ## bw (bandwidth)
    if(class(bw) == "numeric") {
      check.length(bw, 1, " must be either a single numeric value or a function.")
      bw <- round(bw)
    } else {
      check.class(bw, "function", " must be either a single numeric value or a function.")
    }

    ## BHATTACHARYYA COEFFICIENT
    ## sum(sqrt(x relative counts in bin_i * y relative counts in bin_i))

    ## Setting the right number of bins (i)
    if(class(bw) == 'function') {
      ## Bin width
      band.width <- bw(c(x, y), ...)
      ## Bin breaks
      ## adding an extra bandwith to the max to be sure to include all the data
      bin.breaks <- seq(from = min(c(x, y)), to = max(c(x, y) + band.width), by = band.width)
      ## Number of bins
      bin.n <- length(bin.breaks) - 1
    } else {
      ## Bin breaks
      bin.breaks <- graphics::hist(c(x, y), breaks = bw, plot = FALSE)$breaks
      ## Bin width
      band.width <- diff(bin.breaks)[1]
      ## Number of bins
      bin.n <- bw
    }

    ## Counting the number of elements per bin
    histx <- graphics::hist(x, breaks = bin.breaks, plot = FALSE)[[2]]
    histy <- graphics::hist(y, breaks = bin.breaks, plot = FALSE)[[2]]
    ## Relative counts
    rel.histx <- histx / sum(histx)
    rel.histy <- histy / sum(histy)

    ## Calculating the Bhattacharyya Coefficient (sum of the square root of the multiple of the relative counts of both distributions)
    bhatt.coeff <- sum(sqrt(rel.histx * rel.histy))
    return(bhatt.coeff)
  }

  check.class <- function (object, class, msg, errorif = FALSE) {
    match_call <- match.call()
    class_object <- class(object)
    length_class <- length(class)
    if (missing(msg)) {
      if (length_class != 1) {
        msg <- paste(" must be of class ", paste(class, collapse = " or "),
                     ".", sep = "")
      }
      else {
        msg <- paste(" must be of class ", class, ".", sep = "")
      }
    }
    if (length_class != 1) {
      error <- NULL
      for (counter in 1:length_class) {
        if (errorif != TRUE) {
          if (class_object != class[counter]) {
            error <- c(error, TRUE)
          }
          else {
            error <- c(error, FALSE)
          }
        }
        else {
          if (class_object == class[counter]) {
            error <- c(error, TRUE)
          }
          else {
            error <- c(error, FALSE)
          }
        }
      }
      if (!any(!error)) {
        stop(match_call$object, msg, call. = FALSE)
      }
      else {
        return(class_object)
      }
    }
    else {
      if (errorif != TRUE) {
        if (class_object != class) {
          stop(match_call$object, msg, call. = FALSE)
        }
      }
      else {
        if (class_object == class) {
          stop(match_call$object, msg, call. = FALSE)
        }
      }
    }
  }

  check.length <- function (object, length, msg, errorif = FALSE) {
    match_call <- match.call()
    if (errorif != TRUE) {
      if (length(object) != length) {
        stop(match_call$object, msg, call. = FALSE)
      }
    }
    else {
      if (length(object) == length) {
        stop(match_call$object, msg, call. = FALSE)
      }
    }
  }

  #
  #
  #
  #

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
  } else if (test == "<") {
    return(round(sum(dist1 < dist2) / length(dist1), round))
  } else if (test == ">") {
    return(round(sum(dist1 > dist2) / length(dist1), round))
  } else if (test == ">=") {
    return(round(sum(dist1 >= dist2) / length(dist1), round))
  } else if (test == "bhatt") {
    if (is.element("dispRity", utils::installed.packages()[,1]))
      return(round(dispRity::bhatt.coeff(dist1, dist2), round))
    else return(round(bhatt.coeff(dist1, dist2), round))
  }

  warning('
Have you checked that the logical operator is "bhatt", "<", "<=", ">"" or ">="?
          Otherwise the function will not work properly.')
  return(NULL)
}
