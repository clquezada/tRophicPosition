
#' Function to build a JAGS-based Bayesian model to calculate trophic position
#'
#' @param data
#' @param n.chains
#' @param model.string
#' @param n.adapt
#'
#' @return
#' @export
#'
#' @examples

TPmodel <- function (data = NULL,
                     model.string = NULL,
                     n.chains = 2,
                     n.adapt = 10000)
                     {

  model <- rjags::jags.model(textConnection(model.string),
                             data = data,
                             n.chains = n.chains,
                             n.adapt = n.adapt)

  #Here we check if the model has the class required
  if (class(model) == "jags") {
    return (model)

  } else {

    stop("Check the data, the model was not OK")
    return(NULL)
  }

}
