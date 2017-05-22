#' Plot credibility intervals and central tendency descriptor from posterior
#' distributions of trophic position and/or alpha
#'
#' @param df data.frame at least with 4 columns, a grouping variable, maximum,
#' minimum and a central tendency descriptor (median, mode).
#' @param x string defining the grouping variable.
#' @param plotAlpha logical. If TRUE it expects that the data.frame has at least
#' 7 columns, another descriptor of central tendency, its maximum and minimum.
#' @param y1 string of the column with the central tendency descriptor of
#' trophic position. By default is the mode.
#' @param y1min lower value plotted for trophic position. For the credibility
#' interval, this value would be 0.025 percentil.
#' @param y1max higher value plotted for trophic position. For the credibility
#' interval, this value would be 0.975 percentil.
#' @param y1lim vector of length 2, with limits of the y axis of trophic
#' position.
#' @param y2 string of the column with the central tendency descriptor of alpha.
#' @param y2min lower value plotted for alpha. For the credibility interval,
#' this value would be percentil 0.025.
#' @param y2max higher value plotted for alpha. For the credibility interval,
#' this value would be percentil 0.0975.
#' @param xlab string of the label of the X axis.
#' @param ylab1 string of the label of Y1 axis (trophic position).
#' @param ylab2 string of the label of Y2 axis (alpha).
#' @param ... additional parameters passed to credibilityIntervals().
#'
#' @return a gtable (if alpha is plotted) with two ggplot objects
#' a ggplot object (if alpha is not plotted)
#' @export
#'
#' @examples
#' isotopeData <- generateTPData()
#' models <- multiModelTP(isotopeData, n.adapt = 500, n.iter = 500,
#' burnin = 500)
#' credibilityIntervals(models$gg, x = "model")

credibilityIntervals <- function (df, x = "species", plotAlpha = TRUE,
                                  y1 = "mode", y1min = "lower", y1max = "upper",
                                  y1lim = NULL,
                                  y2 = "alpha.median", y2min = "alpha.lower",
                                  y2max = "alpha.upper",
                                  xlab = "Bayesian models",
                                  ylab1 = "Posterior trophic position",
                                  ylab2 = "Posterior alpha",
                                  ...) {

  if(class(df) == "posteriorTP")
    stop("Not implemented yet (posteriorTP")

  else if(class(df) == "posteriorAlpha")
    stop("Not implemented yet (posteriorAlpha")

  else if(class(df) == "list" &
          class(df[1]) == "posteriorTP" &
          class(df[2]) == "posteriorAlpha")
    stop("Not implemented yet (posteriorTP & posteriorAlpha")

  p1 <- ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y1, ymin = y1min,
                       ymax = y1max)) +
    ggplot2::geom_pointrange(size = 1, shape = 16, colour = "grey50") +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab1)

  if(!is.null(y1lim)) p1 <- p1 + ggplot2::ylim(y1lim)

  p1 <- p1 + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_text(size = 8),
                            axis.title = ggplot2::element_text(size = 10),
                            axis.title.x = ggplot2::element_blank())

  p2 <- ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y2, ymin = y2min,
                                                ymax = y2max)) +
    ggplot2::geom_pointrange(size = 1, shape = 16, colour = "grey50") +
    ggplot2::ylab(ylab2) + ggplot2::xlab(xlab) + ggplot2::ylim(c(0,1)) +
    ggplot2::geom_hline(yintercept=0.5, linetype="dashed", colour = "grey50",
                        size=.5) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1, size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.title = ggplot2::element_text(size = 10))

  if (isTRUE(!plotAlpha)) print(p1)
  else gridExtra::grid.arrange(p1, p2, nrow = 2, heights = c(5,5))

}


#   function(df, x, y, lower, upper, ...) {
#
#   dots <- list(...)
#   for (x in dots) print( x)
#
#   ggplot(df, aes(x = x, y = y, ymin = lower, ymax = upper)) +
#     geom_pointrange(size = 1, shape = 21, fill = "grey50", colour = "grey50") +
#     theme_bw() +
#     ylab("Posterior Trophic Position") + xlab("Site") +
#     theme(axis.text.x = element_blank(),
#           axis.text.y = element_text(size = 12),
#           axis.title.x = element_blank(),
#           axis.title = element_text(size = 14))
#
# }
