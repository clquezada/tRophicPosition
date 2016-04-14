#' Title
#'
#' @param isotopeData
#'
#' @return
#' @export
#'
#' @examples
#'
screenIsotopeData1source <- function (isotopeData = NULL, type = "histogram") {

  if (!is.null(isotopeData)){

    if (type == "histogram"){

      if (min(isotopeData$dNb1) < min(isotopeData$dNsc)) {

        xlimMin = min(isotopeData$dNb1)

      } else {
        xlimMin = min(isotopeData$dNsc)
      }

      if (max(isotopeData$dNsc) > max(isotopeData$dNb1)) {

        xlimMax = max(isotopeData$dNsc)

      } else {

        xlimMax = max(isotopeData$dNb1)

      }

      extra = abs(xlimMax - xlimMin) * 0.3

      if (length(isotopeData$dNb1) >= length(isotopeData$dNsc)) {

        hist(isotopeData$dNb1, xlim = c(xlimMin - extra, xlimMax + extra),
             col=rgb(0, 1, 0,0.5), xlab = "isotopic value of Nitrogen",
             main = "Basic histogram of baseline (green)\n and secondary consumer (red)")

        hist(isotopeData$dNsc, col=rgb(1, 0, 0,0.5), add = T)

      } else {

        hist(isotopeData$dNsc, xlim = c(xlimMin - extra, xlimMax + extra),
             col=rgb(1, 0, 0,0.5), xlab = "isotopic value of Nitrogen",
             main = "Basic histogram of baseline (green)\n and secondary consumer (red)")

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
