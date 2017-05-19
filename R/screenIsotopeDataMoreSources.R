#' Internal function that plots the data
#'
#' This function is intended to be used within the package. For details refer to
#' \code{\link{screenIsotopeData}}.
#'
#' @param isotopeData a named list composed at least of 6 vectors, dNb1, dCb1,
#' dCc, dNc, deltaN and deltaC.
#' @param density a string character that might plot the density function.
#' @param baselines integer defining the number of baselines (1, 2 or 3).
#' @param consumer
#' @param b1
#' @param b2
#' @param legend
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @note
#'

screenIsotopeDataMoreSources <- function (isotopeData = NULL,
                                          density = "both",
                                          baselines = 2,
                                          consumer = consumer,
                                          b1 = b1,
                                          b2 = b2,
                                          legend = legend,
                                          title = NULL,
                                          ...) {

  #library(RColorBrewer)
  #library(ggplot2)
  #library(gridExtra)
  #library(ggExtra)

  # Since this function is called from inside the package, we already checked
  # that IsotopeData is not null. But still we check this in case we decide the
  # user can use this function from outside.
  if (!is.null(isotopeData)) {

    #First we create a dataframe, reordering the isotopeData
    #Check the class of isotopeData
    #if (class(isotopeData) == "isotopeData")
    if (baselines == 1){

      if (!is.null(consumer)) df <- toStacked(isotopeData, baselines = 1, consumer)
      else  df <- toStacked(isotopeData, baselines = 1)

      #And now we calculate mean and standard deviation for baselines and consumer
      df.b1 <- df[which(df$Factor == b1), c("d13C","d15N")]
      a <- lapply(df.b1, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b1, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b1.meansSDs <- data.frame(a, b, Factor = b1)

      if (!is.null(consumer)) df.sc <- df[which(df$Factor == consumer),c("d13C","d15N")]
      else df.sc <- df[which(df$Factor == consumer),c("d13C","d15N")]

      a <- lapply(df.sc, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.sc, sd)
      names(b) <- c("sd_d13C", "sd_d15N")

      if (!is.null(consumer)) sc.meansSDs <- data.frame(a, b, Factor = consumer)
      else sc.meansSDs <- data.frame(a, b, Factor = consumer)

      df2 <- rbind(sc.meansSDs, b1.meansSDs)

    } else if (baselines == 2){

      if (!is.null(consumer)) df <- toStacked(isotopeData, baselines = 2,
                                              consumer = consumer, b1 = b1,
                                              b2 = b2)
      else df <- toStacked(isotopeData, baselines = 2, consumer = "Consumer",
                           b1 = b1,
                           b2 = b2)

      #And now we calculate mean and standard deviation for baselines and consumer
      df.b1 <- df[which(df$Factor == b1), c("d13C","d15N")]
      a <- lapply(df.b1, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b1, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b1.meansSDs <- data.frame(a, b, Factor = b1)

      df.b2 <- df[which(df$Factor == b2), c("d13C","d15N")]
      a <- lapply(df.b2, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b2, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b2.meansSDs <- data.frame(a, b, Factor = b2)

      if (!is.null(consumer)) df.sc <- df[which(df$Factor == consumer),
                                          c("d13C","d15N")]
      else df.sc <- df[which(df$Factor == consumer), c("d13C","d15N")]

      a <- lapply(df.sc, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.sc, sd)
      names(b) <- c("sd_d13C", "sd_d15N")

      if (!is.null(consumer)) sc.meansSDs <- data.frame(a, b, Factor=consumer)
      else sc.meansSDs <- data.frame(a, b, Factor=consumer)

      df2 <- rbind(sc.meansSDs, b1.meansSDs, b2.meansSDs)

    } else if (baselines == 3) {


      if (!is.null(consumer)) df <- toStacked(isotopeData, baselines = 3, consumer = consumer)
      else df <- toStacked(isotopeData, baselines = 3)

      #And now we calculate mean and standard deviation for baselines and consumer
      df.b1 <- df[which(df$Factor==b1),c("d13C","d15N")]
      a <- lapply(df.b1, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b1, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b1.meansSDs <- data.frame(a, b, Factor = b1)

      df.b2 <- df[which(df$Factor == b2), c("d13C","d15N")]
      a <- lapply(df.b2, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b2, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b2.meansSDs <- data.frame(a, b, Factor = b2)

      df.b3 <- df[which(df$Factor=="Baseline 3"), c("d13C","d15N")]
      a <- lapply(df.b3, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.b3, sd)
      names(b) <- c("sd_d13C", "sd_d15N")
      b3.meansSDs <- data.frame(a, b, Factor="Baseline 3")

      if (!is.null(consumer)) df.sc <- df[which(df$Factor == consumer), c("d13C","d15N")]
      else df.sc <- df[which(df$Factor==consumer), c("d13C","d15N")]

      a <- lapply(df.sc, mean)
      names(a) <- c("mean_d13C", "mean_d15N")
      b <- lapply(df.sc, sd)
      names(b) <- c("sd_d13C", "sd_d15N")

      if (!is.null(consumer)) sc.meansSDs <- data.frame(a, b, Factor=consumer)
      else sc.meansSDs <- data.frame(a, b, Factor=consumer)

      df2 <- rbind(sc.meansSDs, b1.meansSDs, b2.meansSDs, b3.meansSDs)
    }

    #Here we set up the title and x and y labels (in case that in the future we
    #want to modify through the user. Same applies for the colors)
    xlab <- expression(paste(delta^{13}, "C (\u2030)"))
    ylab <- expression(paste(delta^{15}, "N (\u2030)"))
    palette <- "Set1"
    type <- "qual"

    #At some point we need to take these three ggplots to a function outside

    #get the limits (to set the densities plot limits)
    x_min <- min(df$d13C)
    x_max <- max(df$d13C)

    y_min <- min(df$d15N)
    y_max <- max(df$d15N)

    # if(!is.null(title)) {cat(title); str(title)}

    p0 <- biPlot(df, df2, ylab, xlab, p = "p1", legend = legend)
    p1 <- p0[[3]]

    p2 <- biPlot(df, p = "p2", limits = p0[[1]])

    p3 <- biPlot(df, p = "p3", limits = p0[[2]])

    if (density == "none") {

      p1

    } else if (density == "both") {

      capture.output(temp <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p2, ncol = 2, widths=c(3,1)),
                                      gridExtra::arrangeGrob(p1, p3, ncol=2, widths=c(3,1)),
                                      heights=c(1,3), top = title))
      print(temp)

      } else if (density == "above") {

        gridExtra::grid.arrange(gridExtra::arrangeGrob(p2, p1, nrow=2,
                                                       heights=c(1,3)),
                                top = title)

      } else if (density == "right") {

        gridExtra::grid.arrange(gridExtra::arrangeGrob(p1, p3, ncol=2,
                                                       widths=c(3,1)),
                                top = title)

      } else {

        warning("We expect that density has only four values:
                'none', 'both', 'above' or 'right'.")}

  }

}
