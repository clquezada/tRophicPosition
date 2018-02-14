#' Returns a JAGS-based Bayesian model to use within tRophicPosition.
#'
#' This function returns a string with a Bayesian model to be used with trophic
#' position calculations
#'
#' @param model string. Can be "oneBaseline", "twoBaselines" or
#'   "twoBaselinesFull" at the moment.
#' @param ... additional arguments passed to \code{\link{jagsOneBaseline}},
#'   \code{\link{jagsTwoBaselines}} or \code{\link{jagsTwoBaselinesFull}}.
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

  arguments <- list(...)
  # print(length(arguments))
  # print(arguments)
  # print(names(arguments))
  # #message(names(as.list(match.call())[-1]))

  #oneBaseline priors
  # priorsOB <- c("muB", "sigmaB", "muDeltaN","sigmaDeltaN", "sigma", "TP")

  # flag_dnorm <- FALSE
  # flag_dunif <- FALSE
  # flag_dbeta <- FALSE
  #
  # for (argument in arguments){
  #   #print(typeof(arguments$argument))
  #   if(!grepl("dnorm", argument[[1]])) flag_dnorm <- TRUE
  #   if(!grepl("dunif", argument[[1]])) flag_dunif <- TRUE
  #   if(!grepl("dbeta", argument[[1]])) flag_dbeta <- TRUE
  # }
  #
  # if (flag_dnorm | flag_dunif | flag_dbeta)
  #   warning(
  #     "It seems that you are not using dnorm(mean, sd),  dunif(min, max)
  # or dbeta(a, b) as priors, or they are not correctly written. Please check
  # the arguments")
  #   # message(strwrap("It seems that you are not using dnorm(mean, sd), \n
  #   # dunif(min, max) or dbeta(a, b) as prior, are you sure?"))

  if (is.null(model))
    return(jagsTwoBaselinesFull(...))

  else if (model == "oneBaseline")
    return(jagsOneBaseline(...))

  else if (model == "twoBaselines")
    return(jagsTwoBaselines(...))

  else if (model == "twoBaselinesFull")
    return(jagsTwoBaselinesFull(...))

}
