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
#' @param quiet logical value to indicate whether messages generated during
#'   compilation will be suppressed, as well as the progress bar during
#'   adaptation.
#' @param ... additional arguments passed to \code{\link[rjags]{jags.model}}.
#'
#' @return \code{TPmodel} returns an object inheriting from class jags which can
#'   be used to generate dependent samples from the posterior distribution of
#'   the parameters
#' @export
#'
#' @examples
#' \dontrun{
#' isotopeData <- generateTPData()
#' model.string <- jagsBayesianModel()
#' model <- TPmodel(data = isotopeData, model.string = model.string,
#' n.adapt = 500)
#' }

TPmodel <- function (data = NULL,
                     model.string = NULL,
                     n.chains = 2,
                     n.adapt = 10000,
                     #inits = NULL,
                     quiet = FALSE,
                     ...)
                     {

  if (length(methods::is(model.string)) == 2)
    if (methods::is(model.string)[1] == "oneBaseline")
      data[names(data) %in% c("dCb1", "dNb2", "dCb2", "deltaC", "dCc")] <- NULL

  con <- textConnection(model.string)

  model <- invisible(rjags::jags.model(con,
                             data = data,
                             n.chains = n.chains,
                             n.adapt = n.adapt,
                             #inits = inits,
                             quiet = quiet))
  close.connection(con)

  #Here we check if the model has the class required
  if (methods::is(model)[1] == "jags") {
    return (model)

  } else {

    stop("Check the data, the model was not OK")
    return(NULL)
  }

}
