#' Plot credibility intervals and median from posterior distributions
#'
#' @param df
#' @param x
#' @param plotAlpha
#' @param y1
#' @param y1min
#' @param y1max
#' @param y1lim
#' @param y2
#' @param y2min
#' @param y2max
#' @param xlab
#' @param ylab1
#' @param ylab2
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

credibilityIntervals <- function (df, x = "species", plotAlpha = TRUE,
                                  y1 = "median", y1min = "lower", y1max = "upper",
                                  y1lim = NULL,
                                  y2 = "alpha.median", y2min = "alpha.lower",
                                  y2max = "alpha.upper",
                                  xlab = "Bayesian models",
                                  ylab1 = "Posterior Trophic Position",
                                  ylab2 = "Posterior alpha",
                                  ...) {

  require(ggplot2)

  if(class(df) == "posteriorTP")
    doSomething()

  else if(class(df) == "posteriorAlpha")
    doSomethingElse()

  else if(class(df) == "list" &
          class(df[1]) == "posteriorTP" &
          class(df[2]) == "posteriorAlpha")
    doThis()

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
