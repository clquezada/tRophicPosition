#' Function to create a JAGS-based Bayesian model to calculate trophic position
#'
#' This function is a wrapper of \code{\link[rjags]{jags.model}}. It receives a
#' named list containing the data, a model string returned by either
#' \code{\link{jagsOneBaseline}}, \code{\link{jagsTwoBaselines}} or
#' \code{\link{jagsTwoBaselinesFull}}, and creates a JAGS model object.
#'
#' @param data a list containing the data
#' @param n.chains number of parallel chains for the model
#' @param model.string model string containing a description of the model
#' @param n.adapt number of iterations for adaptation (initial sampling phase)
#'
#' @return \code{TPmodel} returns an object inheriting from class jags which
#' can be used to generate dependent samples from the posterior distribution of
#' the parameters
#' @export
#'
#' @examples

TPmodel <- function (data = NULL,
                     model.string = NULL,
                     n.chains = 2,
                     n.adapt = 10000,
                     inits = NULL, ...)
                     {

  model <- rjags::jags.model(textConnection(model.string),
                             data = data,
                             n.chains = n.chains,
                             n.adapt = n.adapt,
                             inits = inits)

  #Here we check if the model has the class required
  if (class(model) == "jags") {
    return (model)

  } else {

    stop("Check the data, the model was not OK")
    return(NULL)
  }

}
