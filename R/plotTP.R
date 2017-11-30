#' Function to plot a trophic position distribution
#'
#' Wrapper of {\link[SIBER]{siberDensityPlot}}.
#'
#' @param TPdist vector. One posterior distribution (or a collection) of trophic position.
#' In case of wanting to plot two or more posterior distributions, needs to be
#' passed as a {\link[base]{data.frame}} object.
#' @param ... additional arguments passed to this function.
#'
#' @return A new figure window
#' @export
#'
#' @examples
#'species1 <- stats::rnorm(1000, 4, 0.1)
#'species2 <- stats::rnorm(1000, 3, 0.8)
#'plotTP(data.frame(species1, species2))
#'
#'

plotTP <- function (TPdist = NULL, ...) {
  siberDensityPlot <- function (dat, probs = c(95, 75, 50), xlab = "Group", ylab = "Value",
                                xticklabels = NULL, yticklabels = NULL, clr = grDevices::gray((9:1)/10),
                                scl = 1, xspc = 0.5, prn = F, ct = "mode", ylims = NULL,
                                lbound = -Inf, ubound = Inf, main = "", ylab.line = 2, ...)
  {
    n <- ncol(dat)
    if (is.null(ylims)) {
      ylims <- c(min(dat) - 0.1 * min(dat), max(dat) + 0.1 *
                   (max(dat)))
    }
    graphics::plot(1, 1, xlab = "", ylab = "", main = main,
                   xlim = c(1 - xspc, n + xspc), ylim = ylims, type = "n",
                   xaxt = "n", ...)
    graphics::title(xlab = xlab)
    graphics::title(ylab = ylab, line = ylab.line)
    if (is.null(xticklabels)) {
      graphics::axis(side = 1, at = 1:n, labels = (as.character(names(dat))))
    }
    else {
      graphics::axis(side = 1, at = 1:n, labels = (xticklabels))
    }
    clrs <- rep(clr, 5)
    for (j in 1:n) {
      temp <- hdrcde::hdr(dat[, j], probs, h = stats::bw.nrd0(dat[,
                                                                  j]))
      line_widths <- seq(2, 20, by = 4) * scl
      bwd <- c(0.1, 0.15, 0.2, 0.25, 0.3) * scl
      if (prn == TRUE) {
        cat(paste("Probability values for Column", j, "\n"))
        cat(paste("\t", "Mode", format(temp$mode, digits = 3,
                                       scientific = F), "Mean", format(mean(dat[, j]),
                                                                       digits = 3, scientific = F), "Median", format(stats::median(dat[,
                                                                                                                                       j]), digits = 3, scientific = F), "\n"))
      }
      for (k in 1:length(probs)) {
        temp2 <- temp$hdr[k, ]
        graphics::polygon(c(j - bwd[k], j - bwd[k], j +
                              bwd[k], j + bwd[k]), c(max(c(min(temp2[!is.na(temp2)]),
                                                           lbound)), min(c(max(temp2[!is.na(temp2)]), ubound)),
                                                     min(c(max(temp2[!is.na(temp2)]), ubound)), max(c(min(temp2[!is.na(temp2)]),
                                                                                                      lbound))), col = clrs[k])
        if (ct == "mode") {
          graphics::points(j, temp$mode, pch = 19)
        }
        if (ct == "mean") {
          graphics::points(j, mean(dat[, j]), pch = 19)
        }
        if (ct == "median") {
          graphics::points(j, stats::median(dat[, j]),
                           pch = 19)
        }
        if (prn == TRUE) {
          cat(paste("\t", probs[k], "% lower =", format(max(min(temp2[!is.na(temp2)]),
                                                            lbound), digits = 3, scientific = FALSE),
                    "upper =", format(min(max(temp2[!is.na(temp2)]),
                                          ubound), digits = 3, scientific = FALSE),
                    "\n"))
        }
      }
    }
  }

  siberDensityPlot(TPdist, ylab = "Trophic Position", ...)


}
