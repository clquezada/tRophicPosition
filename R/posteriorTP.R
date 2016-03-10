
#' Function to generate posterior samples of trophic position
#'
#' @param model
#' @param variable.names
#' @param n.iter
#'
#' @return
#' @export
#'
#' @examples

posteriorTP <- function (model,
                     variable.names = c("TP", "muDeltaN"),
                     n.iter = 10000)
                     {

  posterior <- rjags::coda.samples(model,
                                   variable.names = variable.names,
                                   n.iter = n.iter)

  #Here we check if the model has the class required
  if (class(posterior) == "mcmc.list") {
    return (posterior)

  } else {

    stop("Check the model, posterior samples weren't generated")
    return(NULL)
  }

}
