#' Title
#'
#' @param aminoacid
#' @param Spp
#' @param TDF
#' @param TDF_sd
#' @param nBeta
#' @param Beta
#' @param Beta_sd
#' @param iter
#' @param adapt_delta
#'
#' @return value
#' @export
#'
#'
# @examples not run
#'

stanOneBaselineCSIA <- function(aminoacid = NULL,
                                Spp = NULL,
                                TDF = NULL,
                                TDF_sd = 1.2,
                                nBeta = 32,
                                Beta = 3.4,
                                Beta_sd = 0.68,
                                iter = 10000,
                                adapt_delta = 0.9) {

  if (!require(brms)) {

    stop("brms not installed")

  }

  data_aa <- data.frame(aminoacid,
                        Spp,
                        TDF, TDF_sd,
                        nBeta, Beta, Beta_sd)

  formula <- brms::bf(aminoacid - stats::rnorm(nBeta, Beta, Beta_sd) ~ Spp:me(TDF, TDF_sd) -1 )
  # print(formula)

  message("###############################\n",
          "CSIA consumer Stan model\n",
          "###############################")

  fit <- brms::brm(formula,
                   data = data_aa,
                   cores = parallel::detectCores(),
                   control = list(adapt_delta = adapt_delta),
                   iter = iter)

  print(summary(fit))

  chira.pos <- 1 + do.call("rbind",
                           lapply(as_draws(fit),
                                  function(j) do.call("cbind",j)))[,1:length(unique(Spp))]


  colnames(chira.pos) <- unique(Spp)

  chira.pos


  }
