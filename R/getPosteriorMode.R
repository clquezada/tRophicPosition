#' Function to get mode from a posterior distribution
#'
#' This function is a wrapper of \code{\link[hdrcde]{hdr}}, it returns one mode
#' (if receives a vector), otherwise it returns a list of modes (if receives a
#' list of vectors). If receives an mcmc object it returns the marginal
#' parameter mode using Kernel density estimation
#' (\code{\link[MCMCglmm]{posterior.mode}}).
#'
#' @param df data frame, list or vector with posterior distribution(s).
#' @param round numeric, number of decimals rounded.
#'
#' @return a vector or a list of modes
#'
#' @export
#'
#' @examples
#' # List example
#' a <- list("First" = rnorm(100,1), "Second" = rnorm(100,2))
#' getPosteriorMode(a)
#'
#' # vector example
#' getPosteriorMode(rnorm(100,5), round = 2)

getPosteriorMode <- function(df = NULL, round = 3) {
  #@importFrom magrittr "%>%" (in the header)
  if (is.null(df)) stop("data frame is NULL")

  if (class(df) == "character") stop("data frame is a string")

  if (class(df) == "mcmc")
    return(round(MCMCglmm::posterior.mode(df), round))

  else if (is.numeric(df)) return(round(hdrcde::hdr(df)[[2]], round))

  else
    a <- lapply(df, hdrcde::hdr)
    a <- lapply(a, function(x) as.numeric(x[[2]]))
    return(data.frame(lapply(a, round, round), row.names = "Posterior mode"))

  }
