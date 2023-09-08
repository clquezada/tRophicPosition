
#' Title
#'
#' @param d15N.bsl
#' @param d15N.con
#' @param Spp
#' @param LANCE
#' @param TDF
#' @param TDF_sd
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples

stanOneBaseline <- function(d15N.bsl = NULL,
                            d15N.con = NULL,
                            Spp = NULL,
                            LANCE = NULL,
                            TDF = 3.4,
                            TDF_sd = 0.51,
                            lambda = 1) {

  if (!require(brms)) {

    stop("brms not installed")

  }

  data.d15N <- data.frame(d15N.bsl)

  data <- data.frame(d15N.con,
                     Spp,
                     LANCE)

  message("###############################\n",
          "d15N baseline Stan model\n",
          "###############################")

  brm.pom <- brm(d15N.bsl ~ 1,
                 data = data.d15N)

  pom.pos <- do.call("rbind",
                     lapply(as_draws(brm.pom),
                            function(j) do.call("cbind",j)))[,1]

  message("###############################\n",
          "d15N consumer Stan model\n",
          "###############################")

  brm.bulk <- brm(d15N.con ~ Spp + (1|LANCE) - 1,
                  data = data)

  con.pos <- do.call("rbind",
                     lapply(as_draws(brm.bulk),
                            function(j) do.call("cbind",j)))[,1:length(unique(Spp))] # agregar nº spp

  sp.sel <- unique(data$Spp)

  colnames(con.pos) <- sp.sel

  #Paso3: Generar TEF posterior distribution
  tef1 <- rnorm(4000,TDF,TDF_sd)

  brm.tef <- data.frame(tef1)

  message("###############################\n",
          "TDF Stan model\n",
          "###############################")
  brm.tef.modelo <- brm(tef1 ~ 1,
                        data = brm.tef)

  #  Remuestreo de la distribucion posterior
  tef.pos <- do.call("rbind",
                     lapply(as_draws(brm.tef.modelo),
                            function(j) do.call("cbind",j)))[,1]

  #Paso 4 calcula TP a partir de las distribuciones a posteriori
  bulk.pos <- lambda + (con.pos - pom.pos)/tef.pos


  }
