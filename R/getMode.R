#' Function to get mode from posterior distribution
#'
#' This function is a wrapper of \code{\link[hdrcde]{hdr}}, it returns one
#' mode (if receives a vector), otherwise returns a lists of modes
#'
#' @param df data.frame with posterior distributions
#'
#' @return a list with modes
#'
#' #@importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' # List example
#' a <- list("First" = rnorm(100,1), "Second" = rnorm(100,2))
#' getPosteriorMode(a)
#'
#' # vector example
#' getPosteriorMode(rnorm(100,5))

getPosteriorMode <- function(df = NULL) {
  if (is.null(df)) stop("data.frame is NULL")

  if(is.numeric(df)) return(round(hdrcde::hdr(df)[[2]], 3))

  else
    a <- lapply(df, hdrcde::hdr)
    a <- lapply(a, function(x) as.numeric(x[[2]]))
    return(lapply(a, round, 3))

  }
