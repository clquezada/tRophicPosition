#' Returns a JAGS Bayesian model to use with tRophicPosition.
#'
#' This function returns a string with a Bayesian model to be used with trophic
#' position calculation
#'
#' @param model string. Can be "oneBaseline", "twoBaselines" or
#' "twoBaselinesFull" at the moment.
#' @param ... additional arguments passed to \code{\link{jagsOneBaseline}},
#' \code{\link{jagsTwoBaselines}} or \code{\link{jagsTwoBaselinesFull}}.
#'
#' @return a jags model as a character string
#' @export
#'
#' @examples
#' # Example with priors for TP.
#' # One baseline Bayesian model with prior for trophic position of consumer
#' # defined as a normal distribution with mean 3 and sd 1
#' model.string <- jagsBayesianModel(model = "oneBaseline", TP = "dnorm(3,1)")
#'
#' # Two baselines model with trophic level of baseline = 1
#' model.string <- jagsBayesianModel(model = "twoBaselines", lambda = 1)
#'
#' # Two baselines full model with priors for alpha
#' model.string <- jagsBayesianModel(model = "twoBaselinesFull",
#' alpha = "dbeta(10,1)")

jagsBayesianModel <- function (model = NULL, ... ) {

  if (is.null(model))
    return(jagsTwoBaselinesFull(...))

  else if (model == "oneBaseline")
    return(jagsOneBaseline(...))

  else if (model == "twoBaselines")
    return(jagsTwoBaselines(...))

  else if (model == "twoBaselinesFull")
    return(jagsTwoBaselinesFull(...))
}
