#' Internal function that plots the data
#'
#' This function is intended to be used within the package. For details refer to
#' \code{\link{screenIsotopeData}}.
#'
#' @param isotopeData a named list composed at least of 6 vectors, dNb1, dCb1,
#' dCsc, dNsc, deltaN and deltaC.
#' @param density a character that might plot the density function.
#' @param baselines integer defining the number of baselines (1, 2 or 3).
#'
#' @return
#' @export
#'
#' @examples
#'
#' @note
#'

screenIsotopeDataMoreSources <- function (isotopeData = NULL,
                                          density = "both",
                                          baselines = 2) {

  #library(RColorBrewer)
  #library(ggplot2)
  #library(gridExtra)
  #library(ggExtra)

  # Since this function is called from inside the package, we already checked
  # that IsotopeData is not null. But still we check this in case we decide the
  # user can use this function from outside.
  if (!is.null(isotopeData)) {

    #First we create a dataframe, reordering the IsotopeData
    #Check the class of IsotopeData
    #if (class(IsotopeData))
    if (baselines == 1){

      df <- toStacked(isotopeData, baselines = 1)

      #And now we calculate mean and standard deviation for baselines and consumer
      df.b1 <- df[which(df$Factor=="Baseline 1"),c("d13C","d15N")]
      a <- lapply(df.b1, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b1, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b1.meansSDs <- data.frame(a, b, Factor="Baseline 1")

      df.sc <- df[which(df$Factor=="Secondary consumer"),c("d13C","d15N")]
      a <- lapply(df.sc, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.sc, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      sc.meansSDs <- data.frame(a, b, Factor="Secondary consumer")

      df2 <- rbind(sc.meansSDs, b1.meansSDs)

    } else if (baselines == 2){

      df <- toStacked(isotopeData, baselines = 2)

      #And now we calculate mean and standard deviation for baselines and consumer
      df.b1 <- df[which(df$Factor=="Baseline 1"), c("d13C","d15N")]
      a <- lapply(df.b1, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b1, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b1.meansSDs <- data.frame(a, b, Factor="Baseline 1")

      df.b2 <- df[which(df$Factor=="Baseline 2"), c("d13C","d15N")]
      a <- lapply(df.b2, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b2, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b2.meansSDs <- data.frame(a, b, Factor="Baseline 2")

      df.sc <- df[which(df$Factor=="Secondary consumer"), c("d13C","d15N")]
      a <- lapply(df.sc, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.sc, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      sc.meansSDs <- data.frame(a, b, Factor="Secondary consumer")

      df2 <- rbind(sc.meansSDs, b1.meansSDs, b2.meansSDs)

    } else if (baselines == 3) {
      df <- toStacked(isotopeData, baselines = 3)

      #And now we calculate mean and standard deviation for baselines and consumer
      df.b1 <- df[which(df$Factor=="Baseline 1"),c("d13C","d15N")]
      a <- lapply(df.b1, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b1, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b1.meansSDs <- data.frame(a, b, Factor="Baseline 1")

      df.b2 <- df[which(df$Factor=="Baseline 2"), c("d13C","d15N")]
      a <- lapply(df.b2, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b2, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b2.meansSDs <- data.frame(a, b, Factor="Baseline 2")

      df.b3 <- df[which(df$Factor=="Baseline 3"), c("d13C","d15N")]
      a <- lapply(df.b3, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b3, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b3.meansSDs <- data.frame(a, b, Factor="Baseline 3")

      df.sc <- df[which(df$Factor=="Secondary consumer"), c("d13C","d15N")]
      a <- lapply(df.sc, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.sc, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      sc.meansSDs <- data.frame(a, b, Factor="Secondary consumer")

      df2 <- rbind(sc.meansSDs, b1.meansSDs, b2.meansSDs, b3.meansSDs)
    }

    #Here we set up the title and x and y labels (in case that in the future we
    #want to modify through the user. Same applies for the colors)
    xlab <- expression(paste(delta^{13}, "C (\u2030)"))
    ylab <- expression(paste(delta^{15}, "N (\u2030)"))
    palette <- "Set1"
    type <- "qual"

    #At some point we need to take these three ggplots to an outside function

    #get the limits (to set the densities plot limits)
    x_min <- min(df$d13C)
    x_max <- max(df$d13C)

    y_min <- min(df$d15N)
    y_max <- max(df$d15N)

    p1 <- biPlot(df, df2, ylab, xlab, p = "p1")

    p2 <- biPlot(df, p = "p2")

    p3 <- biPlot(df, p = "p3")

    if (density == "none") {

      p1

    } else if (density == "both") {

      gridExtra::grid.arrange(gridExtra::arrangeGrob(p2, ncol = 2, widths=c(3,1)),
                              gridExtra::arrangeGrob(p1, p3, ncol=2, widths=c(3,1)),
                              heights=c(1,3))

      } else if (density == "above") {

        gridExtra::grid.arrange(gridExtra::arrangeGrob(p2, p1, nrow=2,
                                                       heights=c(1,3)))

      } else if (density == "right") {

        gridExtra::grid.arrange(gridExtra::arrangeGrob(p1, p3, ncol=2,
                                                       widths=c(3,1)))

      } else {

        warning("We expect that density has only four values:
                'none', 'both', 'above' or 'right'.")}

  }

}
