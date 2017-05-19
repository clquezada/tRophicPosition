#' Function to generate posterior samples of a trophic position JAGS model
#'
#' This is a wrapper of \code{\link[rjags]{coda.samples}} which in turn, is a
#' wrapper of \code{\link[rjags]{jags.samples}}. It extracts random samples from
#'  the posterior distribution of the parameters of a jags model.
#'
#' @param model a JAGS model object returned by any of the models returned by
#' jagsBayesianModel
#' @param variable.names vector of characters giving the names of variables to
#' be monitored
#' @param n.iter integer defining the number of iterations. By default is 10000
#' @param thin
#' @param quiet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

posteriorTP <- function (model,
                     variable.names = c("TP", "muDeltaN"),
                     n.iter = 10000,
                     thin = 10,
                     quiet = FALSE,
                     ...)
                     {

  if (isTRUE(quiet)) progress.bar = "none"
  else progress.bar = "text"

  posterior <- rjags::coda.samples(model,
                                   variable.names = variable.names,
                                   n.iter = n.iter,
                                   thin = thin,
                                   progress.bar = progress.bar, ...)

  #Here we check if the model has the class required
  if (class(posterior) == "mcmc.list") {
    return (posterior)

  } else {

    stop("Check the model, posterior samples weren't generated")
    return(NULL)
  }

}
