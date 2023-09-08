#' Title
#'
#' @param aminoacid aminoacid
#' @param TDF TDF
#' @param TDF_sd TDF_sd
#' @param Spp Spp
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
                                TDF_sd = 1.2) {

  if (!require(brms)) {

    stop("brms not installed")

  }

  data_aa <- data.frame(aminoacid,
                        Spp,
                        TDF, TDF_sd)

  formula <- brms::bf(aminoacid ~ Spp:me(TDF, TDF_sd) -1 )
  # print(formula)

  message("###############################\n",
          "CSIA consumer Stan model\n",
          "###############################")

  fit <- brms::brm(formula,
                   data = data_aa,
                   cores = parallel::detectCores(),
                   control = list(adapt_delta = 0.85))

  print(summary(fit))

  chira.pos <- 1 + do.call("rbind",
                           lapply(as_draws(fit),
                                  function(j) do.call("cbind",j)))[,1:length(unique(Spp))]


  colnames(chira.pos) <- unique(Spp)

  chira.pos


  }
