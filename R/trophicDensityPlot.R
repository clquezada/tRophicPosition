#'Function to plot posterior samples of trophic position estimates
#'
#'This function receives a data frame with sampled posterior trophic position
#'estimates. The user may set if he/she wants quantiles to be plotted, in which
#'case the 95% central, 50% central, mean and median are plotted. Also the user
#'can states whether he/she wants the density plots grouped or not. For
#'visualization purposes, density functions look better if they are not grouped,
#'when quantiles are added.
#'
#'@param df data frame with 2 variables: "TP" and "Species". TP can be posterior
#'  samples of trophic position and Species must be a factor.
#'@param quantiles logical variable. If TRUE 95% and 50% central of the
#'  distribution, plus mean and median are added to the plot.
#'@param grouped logical variable. If TRUE trophic position density plots are
#'  grouped.
#'
#'@return a ggplot2::ggplot object
#'@export
#'
#' @examples
#'species1 <- stats::rnorm(1000, 4, 0.1)
#'species2 <- stats::rnorm(1000, 3, 0.8)
#'TP <- c(species1, species2)
#'Species <- c(rep("Species 1", length(species1)),
#'rep("Species 2", length(species2)))
#'df <- data.frame(TP, Species)
#'trophicDensityPlot(df)

trophicDensityPlot <- function (df = NULL, quantiles = FALSE, grouped = TRUE) {

  # Stupid CRAN fix for variables - see here http://stackoverflow.com/questions/
  # 9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-
  # notes-when
  # As seen in https://github.com/andrewcparnell/simmr/blob/master/R/plot.simmr_
  # output.R
  TP <- TP.median <- TP.025 <- TP.975 <- TP.25 <- TP.75 <- Species <-  NULL

  if (!grouped) {

    g <- ggplot2::facet_grid(Species ~ .)

  }

  if (quantiles) {
    Species.stats <- plyr::ddply(df, "Species", plyr::summarise,
                                 TP.mean = mean(TP),
                                 TP.median=stats::median(TP),
                                 TP.025 = stats::quantile(TP, .025),
                                 TP.25 = stats::quantile(TP, .25),
                                 TP.75 = stats::quantile(TP, .75),
                                 TP.975 = stats::quantile(TP, .975))

    q <- list(ggplot2::geom_vline(ggplot2::aes(xintercept = TP.median),
                                  data = Species.stats, linetype="dotted",
                                  color = "black", size=1),
              ggplot2::geom_vline(ggplot2::aes(xintercept = TP.025),
                                  data = Species.stats, color = "black",
                                  size=1),
              ggplot2::geom_vline(ggplot2::aes(xintercept = TP.975),
                                  data = Species.stats, color = "black",
                                  size=1),
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
