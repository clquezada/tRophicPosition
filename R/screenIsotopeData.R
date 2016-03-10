#' Function to plot and screen Isotope Data with one or two baselines.
#'
#' This function receives a named list of two vectors, and plot a basic histogram.
#'
#' @param TPdata a named list composed of two vectors, dNb1 and dNsc.
#' dNb1 stands for delta Nitrogen of baseline 1, while dNsc stands for
#' delta Nitrogen of secondary consumer.
#'
#' @return
#' @export
#'
#' @examples

screenIsotopeData <- function (TPdata = NULL) {

  if (!is.null(TPdata)){

    if (min(TPdata$dNb1) < min(TPdata$dNsc)) {

      xlimMin = min(TPdata$dNb1)

    } else {
      xlimMin = min(TPdata$dNsc)
    }

    if (max(TPdata$dNsc) > max(TPdata$dNb1)) {

      xlimMax = max(TPdata$dNsc)

    } else {

      xlimMax = max(TPdata$dNb1)

    }

    extra = abs(xlimMax - xlimMin)* 0.2

    hist(TPdata$dNb1, xlim = c(xlimMin - extra, xlimMax + extra),
         col=rgb(0, 1, 0,0.5), xlab = "isotopic value of Nitrogen",
         main = "Basic histogram of baseline (green)\n and secondary consumer (red)")
    hist(TPdata$dNsc, col=rgb(1, 0, 0,0.5), add = T)

  }

}
