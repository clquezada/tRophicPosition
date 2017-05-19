#' Return a JAGS Bayesian model to use with tRophicPosition.
#'
#' Description
#'
#' @param model
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

jagsBayesianModel <- function (model = NULL, ... ) {

  if (is.null(model))
    model_returned <- jagsTwoBaselinesFull(...)

  else if (model == "oneBaseline")
    model_returned <- jagsOneBaseline(...)

  else if (model == "twoBaselines")
    model_returned <- jagsTwoBaselines(...)

  else if (model == "twoBaselinesFull")
    model_returned <- jagsTwoBaselinesFull(...)

  return(model_returned)
}
