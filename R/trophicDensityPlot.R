#' Title
#'
#' @param df
#' @param quantiles
#' @param grouped
#'
#' @return
#' @export
#'
#' @examples

trophicDensityPlot <- function (df = NULL, quantiles = FALSE, grouped = TRUE) {

  if (!grouped) {

    g <- ggplot2::facet_grid(Species ~ .)

  }

  if (quantiles) {
    Species.stats <- plyr::ddply(df, "Species", plyr::summarise, TP.mean = mean(TP),
                           TP.median=stats::median(TP),
                           TP.025 = stats::quantile(TP, .025),
                           TP.25 = stats::quantile(TP, .25),
                           TP.75 = stats::quantile(TP, .75),
                           TP.975 = stats::quantile(TP, .975))

    q <- list(ggplot2::geom_vline(ggplot2::aes(xintercept = TP.median),
                                  data = Species.stats, linetype="dotted",
                                  color = "black", size=1),
              ggplot2::geom_vline(ggplot2::aes(xintercept = TP.025),
                                  data = Species.stats, color = "black", size=1),
              ggplot2::geom_vline(ggplot2::aes(xintercept = TP.975),
                                  data = Species.stats, color = "black", size=1),
              ggplot2::geom_vline(ggplot2::aes(xintercept = TP.25),
                                  data = Species.stats, color = "grey", size=1),
              ggplot2::geom_vline(ggplot2::aes(xintercept = TP.75),
                                  data = Species.stats, color = "grey", size=1))
  }

  if (quantiles & !grouped ) {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = TP, colour = Species,
                                          fill = Species)) +
      ggplot2::geom_density(alpha=0.7) + ggplot2::theme_bw() +
      ggplot2::xlab("Trophic position") + q + g

    } else if (quantiles) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = TP, colour = Species,
                                            fill = Species)) +
        ggplot2::geom_density(alpha=0.7) + ggplot2::theme_bw() +
        ggplot2::xlab("Trophic position") + ggplot2::coord_flip() + q

  } else if (!grouped) {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = TP, colour = Species,
                                          fill = Species)) +
      ggplot2::geom_density(alpha=0.7) + ggplot2::theme_bw() +
      ggplot2::xlab("Trophic position") + g

  } else {

    p <- ggplot2::ggplot(df, ggplot2::aes(x = TP, colour = Species,
                                          fill = Species)) +
      ggplot2::geom_density(alpha=0.7) + ggplot2::theme_bw() +
      ggplot2::xlab("Trophic position") + ggplot2::coord_flip()

  }

  return(p)


}
