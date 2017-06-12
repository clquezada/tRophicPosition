#' Function to create a JAGS-based Bayesian model to calculate trophic position
#'
#' This function is a wrapper of \code{\link[rjags]{jags.model}}. It receives an
#' isotopeData class object containing the data, a model string returned by
#' either \code{\link{jagsOneBaseline}}, \code{\link{jagsTwoBaselines}},
#' \code{\link{jagsTwoBaselinesFull}} or \code{\link{jagsBayesianModel}}, and
#' creates a JAGS model object.
#'
#' @param data a list containing the data.
#' @param n.chains number of parallel chains for the model.
#' @param model.string model string containing a description of the model.
#' @param n.adapt number of iterations for adaptation (initial sampling phase)
#' @param quiet logical value to indicate wheter messages generated during
#'   compilation will be suppressed, as well as the progress bar during
#'   adaptation.
#' @param ... aditional arguments passed to \code{\link[rjags]{jags.model}}.
#'
#' @return \code{TPmodel} returns an object inheriting from class jags which can
#'   be used to generate dependent samples from the posterior distribution of
#'   the parameters
#' @export
#'
#' @examples isotopeData <- generateTPData()
#' model.string <- jagsBayesianModel()
#' model <- TPmodel(data = isotopeData, model.string = model.string,
#' n.adapt = 500)

TPmodel <- function (data = NULL,
                     model.string = NULL,
                     n.chains = 2,
                     n.adapt = 10000,
                     #inits = NULL,
                     quiet = FALSE,
                     ...)
                     {

  if (length(class(model.string)) == 2)
    if (class(model.string)[[2]] == "oneBaseline")
      data[names(data) %in% c("dCb1", "dNb2", "dCb2", "deltaC", "dCc")] <- NULL

  model <- invisible(rjags::jags.model(textConnection(model.string),
                             data = data,
                             n.chains = n.chains,
                             n.adapt = n.adapt,
                             #inits = inits,
                             quiet = quiet))

  #Here we check if the model has the class required
  if (class(model) == "jags") {
    return (model)

  } else {

    stop("Check the data, the model was not OK")
    return(NULL)
  }

}
