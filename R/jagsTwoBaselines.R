#' Defines the jags model to fit the two baseline trophic position model
#'
#' Takes some parameters and returns a jags model object as a
#' character string for passing to \code{\link[rjags.model]{rjags.model}}.
#'
#' @param TPprior
#' @param mubprior
#'
#' @return A jags model as a character string
#'
#' @export

jagsTwoBaselines <- function (TPprior = NULL,
                              mubprior = NULL)
{

  # ----------------------------------------------------------------------------
  # JAGS code for fitting Inverse Wishart version of SIBER to two groups
  # ----------------------------------------------------------------------------

  modelstring <- "

    model {


  }" # end of jags model script

  return(modelstring)

} # end of function
