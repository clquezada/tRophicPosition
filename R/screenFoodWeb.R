#' Function that creates a biplot of a food web with stable isotope values (d13C
#' and d15N)
#'
#' @param df a data frame that contains the isotope values. By defaults, the
#'   data frame needs to have the following columns: d13C, d15N, Species and
#'   FG. Species stands for the scientific name (or common name), and FG stands
#'   for the functional group for each species.
#' @param grouping a vector with the name of the columns (variables) that will
#'   be used to summarize, and plot the data frame.
#' @param printSummary a logical value to indicate whether the summary is
#'   printed
#' @param ... optional arguments that are passed to the function for later use.
#'
#' @return a ggplot2 object with the biplot of the data frame. Also prints the
#'   summary of the data frame as needed.
#'
#' @export
#'
#' @examples
#' data("Bilagay")
#' subset_CHI <- Bilagay[Bilagay[,"Location"] %in% "CHI",]
#' screenFoodWeb(subset_CHI, grouping = c("Spp", "FG"))

screenFoodWeb <- function (df = NULL, grouping = c("Species", "FG"),
                                printSummary = FALSE, ...){
  #require(ggplot2)

  # Stupid CRAN fix for variables - see here http://stackoverflow.com/questions/
  # 9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-
  # notes-when
  # As seen in https://github.com/andrewcparnell/simmr/blob/master/R/plot.simmr_
  # output.R
  meanC <- meanN <- sdN <- sdC <- NULL

  summary <- tRophicPosition::summariseIsotopeData(df, grouping, printSummary)

  shape <- as.numeric(factor(summary[[grouping[2]]])) + 20

  p <- ggplot2::ggplot(summary,
                       ggplot2::aes(meanC, meanN,
                                    fill = factor(summary[[grouping[1]]]))) +
    ggplot2::geom_errorbar(data = summary,
                           ggplot2::aes(ymin = meanN - sdN, ymax = meanN + sdN),
                           width = 0.15,
                           color = "black", size = .5) +
    ggplot2::geom_errorbarh(data = summary,
                            ggplot2::aes(xmin = meanC - sdC,
                                         xmax = meanC + sdC),
                            height = 0.15, color = "black", size = .5) +
    ggplot2::geom_point(size = 4, shape = shape)+
    ggplot2::ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
    ggplot2::labs(fill = grouping[1])+
    #scale_colour_brewer(RColorBrewer::brewer.pal(
    #length(Strangford.summary$Species_name),"Set1"))+
    #title = "Strangford Lough (mean +- sd)") +
    #geom_point(data=Strangford.summary, size=3,
    #shape = Strangford.summary$FG) +
    #scale_shape_discrete(solid = T) +
    ggplot2::theme_bw()

  #if (!is.null(title)) p <- p + labs(title = title)

  print(p)
  p

}
