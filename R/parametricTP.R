#' Parametric trophic position
#'
#' Calculation of old school trophic position
#'
#' @param siData
#' @param lambda
#' @param print
#'
#' @return
#' @export
#'
#' @examples
parametricTP <- function (siData, lambda = 2, print = TRUE) {

  if (class(siData) != "isotopeData") stop ("We need an isotopeData class object")
  cat("Beta version! check the values with your calculations")
  sm <- summary(siData, print = FALSE, round_dec = 4)

  dNb1 <- sm[[1,2]]
  dCb1 <- sm[[2,2]]
  dNb2 <- sm[[3,2]]
  dCb2 <- sm[[4,2]]
  dNc <- sm[[7,2]]
  dCc <- sm[[8,2]]
  deltaN <- sm[[5,2]]
  deltaC <- sm[[6,2]]

  TP <- function(dNc, dNb1, dNb2, deltaN, alpha)
    lambda + ((dNc - ((dNb1 * alpha) + (dNb2 * (1 - alpha))))/deltaN)

  alphaCQR <- function (dCc, dCb1, dCb2, tp)
    ((dCc - (deltaC*tp/lambda)) - dCb2) / (dCb1 - dCb2)

  alphaPost <- function (dCc, dCb1, dCb2, tp)
    (dCb2 - (dCc + (deltaC*tp) ) ) / (dCb2 - dCb1)

  TPoneBaseline <- lambda + (dNc - dNb1)/deltaN
  if (isTRUE(print)) print(paste("One baseline TP: ", round(TPoneBaseline,2)))

  alpha <- (dCc - dCb2) / (dCb1 - dCb2)
  TPTwoBaselines <- TP(dNc, dNb1, dNb2, deltaN, alpha)
  if (isTRUE(print)) print(paste("Two baselines TP: ", round(TPTwoBaselines,2), round(alpha,3)))

  i = 0
  TP_p1 <- TPTwoBaselines
  alpha_p1 <- alphaCQR(dCc, dCb1, dCb2, TP_p1)
  alpha_p2 <- alpha_p1
  TP_p2 <- TP(dNc, dNb1, dNb2, deltaN, alpha_p1)

  if (isTRUE(print)) print(paste("Full model TP. At the beginning: ", round(TP_p1,2),
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
      print(paste("Convergence not reached after ", i, " iterations.", "TP: ", round(TP_p2,2),
            " alpha: ", round(alpha_p2,3)))
    else
      print(paste("Convergence after ", i, " iterations. TP: ", round(TP_p2,2),
                " alpha: ", round(alpha_p2,3)))

  return(list(TPoneBaseline, TPTwoBaselines, alpha, TP_p2, alpha_p2))
  }

  #Post's version // REMOVE AFTER UPLOADING TO GITHUB
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
  #     print(paste("Convergence not reached after ", i, " iterations.", "TP: ", round(TP_p2,2),
  #           " alpha: ", round(alpha_p2,3)))
  #   else
  #     print(paste("Convergence after ", i, " iterations.", "TP: ", round(TP_p2,2),
  #               " alpha: ", round(alpha_p2,3), " difference in alpha: ", alpha_p1-alpha_p2))
  # }

}
