#' Parametric trophic position
#'
#' Calculation of parametric trophic position (with means) partially based on
#' Post (2002: Using Stable Isotopes to Estimate Trophic Position: Models,
#' Methods, and Assumptions. Ecology 83, 703).
#'
#' In case of the one baseline model, trophic position is calculated as \deqn{TP
#' = lambda + ((dNc - dNb1) / deltaN)} where lambda is trophic level of baseline
#' 1, dNc are d15N values of consumer, dNb1 are d15N values of baseline 1 and
#' deltaN are trophic discrimination factor values of N.
#'
#'
#' In case of the two baselines model, trophic position is calculated as
#' \deqn{TP = lambda + ((dNc - ((dNb1 * alpha) + (dNb2 * (1 - alpha)))) /
#' deltaN)} and \deqn{alpha = (dCc - dCb2) / (dCb1 - dCb2)} Additional variables
#' are dCc (d13C values of consumer), dNb2 (d15N values of baseline 2), alpha
#' (relative contribution of N from baseline 1), and dCb1 and dCb2 (d13C values
#' of baselines 1 and 2 respectively).
#'
#'
#' In case of the two baselines full model, trophic position is calculated with
#' the same equation as the two baselines model, but alpha is calculated as
#' \deqn{alpha = ((dCc - (deltaC * TP / lambda)) - dCb2) / (dCb1 - dCb2)} and
#' includes deltaC (trophic discrimination factor for C).
#'
#' In all cases trophic position is calculated based on means of isotope values
#' and trophic discrimination factors. For the two baselines full model, an
#' iteration is needed to get convergence of trophic position, starting with
#' alpha calculated with the two baselines simple model. If no convergence is
#' gotten after 50 iterations a message is plotted and both alpha and trophic
#' position are printed.
#'
#' @param siData an isotopeData class object.
#' @param lambda numerical value representing trophic level of baseline(s).
#' @param print a logical value to indicate whether the output is printed or
#'   not.
#'
#' @return a list with parametric trophic position calculated with a one
#'   baseline model, a two baselines model and its alpha value, and a two
#'   baselines full model and its alpha value.
#' @export
#'
#' @examples
#' consumer <- generateTPData()
#' parametricTP(consumer)
parametricTP <- function (siData, lambda = 2, print = TRUE) {

  if (class(siData) != "isotopeData")
    stop ("We need an isotopeData class object")

  cat("")
  print("***************************************")
  print("Parametric version of trophic position")
  print(paste("For consumer: ", attributes(siData)$consumer))
  sm <- lapply(siData, mean)

  dNb1 <- sm$dNb1
  dCb1 <- sm$dCb1
  dNb2 <- sm$dNb2
  dCb2 <- sm$dCb2
  dNc <- sm$dNc
  dCc <- sm$dCc
  deltaN <- sm$deltaN
  deltaC <- sm$deltaC

  TP <- function(dNc, dNb1, dNb2, deltaN, alpha)
    lambda + ((dNc - ((dNb1 * alpha) + (dNb2 * (1 - alpha))))/deltaN)

  alphaCQR <- function (dCc, dCb1, dCb2, tp)
    ((dCc - (deltaC*tp/lambda)) - dCb2) / (dCb1 - dCb2)

  alphaPost <- function (dCc, dCb1, dCb2, tp)
    (dCb2 - (dCc + (deltaC*tp) ) ) / (dCb2 - dCb1)

  TPoneBaseline <- lambda + ((dNc - dNb1)/deltaN)
  if (isTRUE(print)) print(paste("One baseline TP: ", round(TPoneBaseline,2)))

  if (is.null(dNb2) | is.null(dCb2))
    return(TPoneBaseline)

  alpha <- (dCc - dCb2) / (dCb1 - dCb2)
  TPTwoBaselines <- TP(dNc, dNb1, dNb2, deltaN, alpha)
  if (isTRUE(print)) print(paste("Two baselines TP: ", round(TPTwoBaselines,2),
                                 round(alpha,3)))

  i <- 0
  TP_p1 <- TPTwoBaselines
  alpha_p1 <- alphaCQR(dCc, dCb1, dCb2, TP_p1)
  alpha_p2 <- alpha_p1
  TP_p2 <- TP(dNc, dNb1, dNb2, deltaN, alpha_p1)

  if (isTRUE(print)) print(paste("Full model TP. At the beginning: ",
                                 round(TP_p1,2),
                                 round(alpha_p1,3)))
  while (TP_p1 != TP_p2) {
    TP_p1 <- TP_p2
    alpha_p1 <- alpha_p2
    alpha_p2 <- alphaCQR(dCc, dCb1, dCb2, TP_p1)
    TP_p2 <- TP(dNc, dNb1, dNb2, deltaN, alpha_p2)
    i <- i + 1
    if (i == 50) break
  }

  if(isTRUE(print)) {
    if (i == 50)
      print(paste("Convergence not reached after ", i, " iterations.", "TP: ",
                  round(TP_p2,2),
                  " alpha: ", round(alpha_p2,3)))
    else
      print(paste("Convergence after ", i, " iterations. TP: ", round(TP_p2,2),
                " alpha: ", round(alpha_p2,3)))

  return(list(TPoneBaseline, TPTwoBaselines, alpha, TP_p2, alpha_p2))
  }

  #Post's version // REMOVE BEFORE UPLOADING TO GITHUB
  # i = 0
  # TP_p1 <- TPTwoBaselines
  # alpha_p1 <- alphaPost(dCc, dCb1, dCb2, TP_p1)
  # alpha_p2 <- alpha_p1
  # TP_p2 <- TP(dNc, dNb1, dNb2, deltaN, alpha_p1)
  #
  # if (isTRUE(print)) print(paste("Post's full model. At the beginning, TP: ",
  #                                round(TP_p1,2),
  #                                " alpha: ", round(alpha_p1,3)))
  # while (TP_p1 != TP_p2) {
  #   TP_p1 <- TP_p2
  #   alpha_p1 <- alpha_p2
  #   alpha_p2 <- alphaPost(dCc, dCb1, dCb2, TP_p1)
  #   TP_p2 <- TP(dNc, dNb1, dNb2, deltaN, alpha_p2)
  #   i <- i + 1
  #   if (i == 50) break
  # }
  #
  # if(isTRUE(print)) {
  #   if (i == 50)
  #     print(paste("Convergence not reached after ", i, " iterations.", "TP: ",
  #      round(TP_p2,2),
  #           " alpha: ", round(alpha_p2,3)))
  #   else
  #     print(paste("Convergence after ", i, " iterations.", "TP: ",
  #     round(TP_p2,2),
  #               " alpha: ", round(alpha_p2,3), " difference in alpha: ",
  #                alpha_p1-alpha_p2))
  # }

}
