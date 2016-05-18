#' Return a JAGS model with one to three baselines.
#'
#' To do.
#'
#' @param ...
#' @param nb integer, numbers of baselines required.
#'
#' @return
#' @export
#'
#' @examples

jagsTPmodels <- function (nb = 1, ...) {

  if (nb == 1) {

    return( jagsOneBaseline(...) )

  } else if (nb == 2) {

    return( jagsTwoBaselines(...) )

  } else if (nb == 3) {

    return( jagsThreeBaselines(...) )

    }


}
