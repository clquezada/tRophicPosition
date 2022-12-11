#' Plot credibility intervals and central tendency descriptor from posterior
#' distributions of trophic position and/or alpha parameter
#'
#' This function plots a data frame in ggplot2 format (variables in columns,
#' observations in rows), likely returned by the functions
#' \code{\link{multiModelTP}} and \code{\link{multiSpeciesTP}}. This is
#' especially useful when there are several species or communities to compare,
#' and a combined plot is preferred.
#'
#' @param df data frame with at least 4 columns, a grouping variable, maximum,
#'   minimum and a central tendency descriptor (median, mode, etc.).
#' @param x string defining the grouping variable.
#' @param plotAlpha logical. If TRUE it expects that the data frame has at least
#'   7 columns, another descriptor of central tendency, its maximum and minimum.
#' @param legend list, position of the legend if not NULL, e.g. c(0.8, 0.8).
#' @param y1 string of the column with the central tendency descriptor of
#'   trophic position. By default, is the mode.
#' @param y1min lower value plotted for trophic position. For the 95%
#'   credibility interval, this value would be 0.025 percentile.
#' @param y1max higher value plotted for trophic position. For the 95%
#'   credibility interval, this value would be 0.975 percentile.
#' @param y1lim vector of length 2, with limits of the y axis of trophic
#'   position.
#' @param y2 string of the column with the central tendency descriptor of alpha.
#' @param y2min lower value plotted for alpha. For the 95% credibility interval,
#'   this value would be percentile 0.025.
#' @param y2max higher value plotted for alpha. For the 95% credibility
#'   interval, this value would be percentile 0.0975.
#' @param xlab string of the label of the X axis.
#' @param ylab1 string of the label of Y1 axis (trophic position).
#' @param ylab2 string of the label of Y2 axis (alpha).
#' @param group_by grouping variable (factor) in case of using colours.
#' @param scale_colour_manual a list of colours (ggplot2 syntaxis) to use with
#' group_by.
#' @param plot logical, by default TRUE. In case of saving the output as a
#' variable, the user can decide not to plot the output.
#' @param ... additional parameters passed to credibilityIntervals().
#' @param legendAlpha list, position of the legend for the alpha plot, if not
#'  NULL, e.g. c(0.8, 0.8).
#' @param labels string, manual labels for the x axis.
#'
#' @return a gtable (if alpha is plotted) with two ggplot2 objects or a ggplot2
#'   object (if alpha is not plotted)
#' @export
#'
#' @examples
#' isotopeData <- generateTPData()
#' models <- multiModelTP(isotopeData, n.adapt = 200, n.iter = 200,
#' burnin = 200)
#' credibilityIntervals(models$gg, x = "model")

credibilityIntervals <- function (df,
                                  x = "consumer",
                                  plotAlpha = TRUE,
                                  legend = NULL,
                                  legendAlpha = NULL,
                                  y1 = "mode",
                                  y1min = "lower",
                                  y1max = "upper",
                                  y1lim = NULL,
                                  y2 = "alpha.mode",
                                  y2min = "alpha.lower",
                                  y2max = "alpha.upper",
                                  xlab = "Bayesian models",
                                  ylab1 = "Posterior trophic position",
                                  ylab2 = "Posterior alpha",
                                  group_by = NULL,
                                  scale_colour_manual = NULL,
                                  labels = NULL,
                                  plot = TRUE,
                                  ...) {

  #if(missing(plot)) plot = TRUE

  if(methods::is(df)[1] == "posteriorTP")
    stop("Not implemented yet (posteriorTP")

  else if(methods::is(df)[1] == "posteriorAlpha")
    stop("Not implemented yet (posteriorAlpha")

  else if(methods::is(df)[1] == "list" &
          methods::is(df[1])[1] == "posteriorTP" &
          methods::is(df[2])[1] == "posteriorAlpha")
    stop("Not implemented yet (posteriorTP & posteriorAlpha")

  if (!is.null(levels(df$model))){
    # print("we are in?")

    levels(df$model) <- sub("^1b$", "one baseline", levels(df$model))
    levels(df$model) <- sub("^2b$", "two baselines", levels(df$model))
    levels(df$model) <- sub("^2bf$", "two baselines full", levels(df$model))

  }

  if (!is.null(group_by)) {

    p1 <- ggplot2::ggplot(df,
                          ggplot2::aes_string(x = x,
                                              y = y1,
                                              ymin = y1min,
                                              ymax = y1max,
                                              colour = group_by)) +
    ggplot2::geom_pointrange(size = 0.8,
                             shape = 16)
    }
  else {
    p1 <- ggplot2::ggplot(df,
                          ggplot2::aes_string(x = x,
                                              y = y1,
                                              ymin = y1min,
                                              ymax = y1max)) +
      ggplot2::geom_pointrange(size = 0.8,
                               shape = 16,
                               colour = "grey50")
  }

  if(!is.null(y1lim)) p1 <- p1 + ggplot2::ylim(y1lim)

  if (!is.null(scale_colour_manual))
    p1 <- p1 + ggplot2::scale_colour_manual(values = scale_colour_manual)

  p1 <- p1 + ggplot2::theme_bw() +
    ggplot2::ylab(ylab1)

  if (is.numeric(legend) & length(legend) == 2 & !is.null(legend))
    p1 <- p1 + ggplot2::theme(legend.position = legend,
                              legend.title = ggplot2::element_blank(),
                              #legend.background = ggplot2::element_rect(
                              #fill="transparent"))
                              legend.background =
                                ggplot2::element_rect(fill = "white",
                                                      size = 0.5,
                                                      linetype = "solid",
                                                      colour = "black"))
  else p1 <- p1 + ggplot2::theme(legend.position = "none")

  if (isTRUE(!plotAlpha)) {
    # When we don't want to plot alpha
    #
    # To do: check if alpha is not NULL or NA
    #if (is.numeric(y2) && is.numeric(y2min) && is.numeric(y2max))

    p1 <- p1 + ggplot2::xlab(xlab) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                         hjust=1, size = 8),
                     axis.text.y = ggplot2::element_text(size = 8,
                                                         margin =
                                                           ggplot2::margin(
                                                           5,2.5,10,5,"pt")
                     ),
                     axis.title = ggplot2::element_text(size = 10))

    if (!is.null(labels)) p1 <- p1 +
        ggplot2::scale_x_discrete(labels = as.character(labels))

    # if (is.numeric(legend) & length(legend) == 2 & !is.null(legend))
    #   p1 <- p1 + ggplot2::theme(legend.position = legend,
    #       legend.title = ggplot2::element_blank(),
    #   #legend.background = ggplot2::element_rect(fill="transparent"))
    #                             legend.background =
    #                             ggplot2::element_rect(fill = "white",
    #                      size = 0.5,
    #                                linetype = "solid",
    #                                colour = "black"))

    } else p1 <- p1 + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                               axis.text.y =
                                 ggplot2::element_text(size = 8,
                                                       margin =
                                                         ggplot2::margin(
                                                           5,2.5,10,5,"pt")
                               ),
                               axis.title = ggplot2::element_text(size = 10),
                               axis.title.x = ggplot2::element_blank())

  if (!is.null(group_by)) {

    p2 <- ggplot2::ggplot(df, ggplot2::aes_string(x = x,
                                                  y = y2,
                                                  ymin = y2min,
                                                  ymax = y2max,
                                                  colour = group_by)) +
      ggplot2::geom_pointrange(size = 0.6,
                               shape = 16) +

      ggplot2::geom_hline(yintercept=0.5,
                          linetype="dashed",
                          size=.5)
  } else {

    p2 <- ggplot2::ggplot(df, ggplot2::aes_string(x = x,
                                                  y = y2,
                                                  ymin = y2min,
                                                  ymax = y2max)) +
      ggplot2::geom_pointrange(size = 0.6,
                               shape = 16,
                               colour = "grey50") +

      ggplot2::geom_hline(yintercept=0.5,
                          linetype="dashed",
                          colour = "grey50",
                          size=.5)
  }

  p2 <- p2 + ggplot2::ylab(ylab2) +
    ggplot2::xlab(xlab) +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1, size = 8),
                   axis.text.y = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10))

  if (!is.null(scale_colour_manual))
    p2 <- p2 + ggplot2::scale_colour_manual(values = scale_colour_manual)

  if (is.numeric(legendAlpha) & length(legendAlpha) == 2 &
      !is.null(legendAlpha))
    p2 <- p2 + ggplot2::theme(legend.position = legend,
                              legend.title = ggplot2::element_blank(),
                              #legend.background =
                              #ggplot2::element_rect(fill="transparent"))
                              legend.background =
                                ggplot2::element_rect(fill = "white",
                                                      size = 0.5,
                                                      linetype = "solid",
                                                      colour = "black"))
  else p2 <- p2 + ggplot2::theme(legend.position = "none")

  if (!is.null(labels)) p2 <- p2 +
    ggplot2::scale_x_discrete(labels = as.character(labels))

  # When we do not want to plot alpha
  if (isTRUE(!plotAlpha)) {
    if (isTRUE(plot)) print(p1)
    return(p1)
    }

  else {
    if (isTRUE(plot)) return(gridExtra::grid.arrange(p1,
                                                     p2,
                                                     nrow = 2,
                                                     heights = c(5,5)))
    # return(list(p1,p2))
    return(gridExtra::grid.arrange(p1,
                                   p2,
                                   nrow = 2,
                                   heights = c(5,5)), newpage = FALSE)
    }

}


#   function(df, x, y, lower, upper, ...) {
#
#   dots <- list(...)
#   for (x in dots) print( x)
#
#   ggplot(df, aes(x = x, y = y, ymin = lower, ymax = upper)) +
#     geom_pointrange(size = 1, shape = 21,
#     fill = "grey50", colour = "grey50") +
#     theme_bw() +
#     ylab("Posterior Trophic Position") + xlab("Site") +
#     theme(axis.text.x = element_blank(),
#           axis.text.y = element_text(size = 12),
#           axis.title.x = element_blank(),
#           axis.title = element_text(size = 14))
#
# }
