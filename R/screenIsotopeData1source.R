#' Internal function that plots the data
#'
#' This function is intended to be used within the package. For details refer to
#' \code{\link{screenIsotopeData}}. Is planned to deprecate it in the near
#' future.
#'
#' @param isotopeData a named list composed at least of 6 vectors, dNb1, dCb1,
#' dCc, dNc, deltaN and deltaC
#' @param type character that states if an "histogram" or a "density" will be
#' ploted.
#'
#' @return
#' @export
#'
#' @examples
#'
screenIsotopeData1source <- function (isotopeData = NULL, type = "histogram") {

  if (!is.null(isotopeData)){

    if (type == "histogram"){

      if (min(isotopeData$dNb1) < min(isotopeData$dNc)) {

        xlimMin = min(isotopeData$dNb1)

      } else {
        xlimMin = min(isotopeData$dNc)
      }

      if (max(isotopeData$dNc) > max(isotopeData$dNb1)) {

        xlimMax = max(isotopeData$dNc)

      } else {

        xlimMax = max(isotopeData$dNb1)

      }

      extra = abs(xlimMax - xlimMin) * 0.3

      if (length(isotopeData$dNb1) >= length(isotopeData$dNc)) {

        hist(isotopeData$dNb1, xlim = c(xlimMin - extra, xlimMax + extra),
             col=rgb(0, 1, 0,0.5), xlab = "isotopic value of nitrogen",
             main = "Basic histogram of baseline (green)\n and consumer (red)")

        hist(isotopeData$dNc, col=rgb(1, 0, 0,0.5), add = T)

      } else {

        hist(isotopeData$dNc, xlim = c(xlimMin - extra, xlimMax + extra),
             col=rgb(1, 0, 0,0.5), xlab = "isotopic value of nitrogen",
             main = "Basic histogram of baseline (green)\n and consumer (red)")

        hist(isotopeData$dNb1, col=rgb(0, 1, 0,0.5), add = T)

        }

    } else if (type == "density") {

      df <- toStacked(isotopeData)

      ggplot2::ggplot(df, aes(x = d15N, colour = Factor, fill = Factor)) +
        ggplot2::geom_density(alpha=0.7) +
        ggplot2::theme_bw() +
        ggplot2::xlab(expression(paste(delta^{15}, "N (\u2030)"))) +
        ggplot2::coord_flip()


    }

    }
  }
