
#' A function to generate some data for trophic position calculation
#'
#' @param n.obs
#' @param n.baselines
#' @param std.dev
#' @param dNb1
#' @param dNsc
#' @param deltaN
#'
#' @return
#' @export
#'
#' @examples

generateTPData <- function (n.obs = 25,
                            std.dev = 0.1,
                            n.baselines = 1,
                            dNb1 = NULL,
                            dNsc = NULL,
                            deltaN = 3.4) {

  # Here we simulate some data for the first baseline
  # If dN of the baseline 1 is NULL, n.obs random numbers are generated
  # within -5 and 5, with a standard deviation std.dev
  if (is.null(dNb1)){
    dNb1 <- rnorm(n.obs, runif(1, -5, 5), std.dev)
  } else {
    # If the user supply dNb1, then n.obs numbers are randomly generated
    # from a normal distribution with mean dNb1 and standard deviation std.dev
    dNb1 <- rnorm(n.obs, dNb1, std.dev)
  }

  # Now we simulate some data for the secondary consumer we want
  # to calculate trophic position of

  # If no dN of secondary consumer is supplied, some data is
  # simulated two trophic positions over dN of baseline
  if (is.null(dNsc)) {
    dNsc <- rnorm(n.obs, dNb1 + (2 * deltaN), std.dev)
  } else {
    #If the user supply dNsc, then n.obs numbers are randomly generated
    #from a normal distribution with mean dNsc and standard deviation std.dev
    dNsc <- rnorm(n.obs, dNsc, std.dev)
  }

  # Finally we simulate some deltaN observed data
  deltaN <- rnorm (20, deltaN, std.dev)

  if (n.baselines == 2){
    # To do
    #dCsc <- rnorm(n.obs, runif(1, -10, -25), 1)
    #dCb1 <- rnorm(n.obs, runif(1, -10, -25), std.dev)

    }


  return(list(dNb1 = dNb1, dNsc = dNsc, deltaN = deltaN))
}
