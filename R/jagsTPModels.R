#' Title
#'
#' @param ...
#' @param nb
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
