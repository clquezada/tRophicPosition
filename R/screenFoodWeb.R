#' Function that creates a biplot of food web with stable isotope values (d13C
#' and d15N)
#'
#' Description
#'
#' @param df a data frame that contains the isotope values. It needs to have the
#'  following columns: d13C, d15N, Species and FG. Species stands for the
#'  scientific name (or common name), and FG stands for the functional group for
#'  each species.
#' @param grouping a vector with the name of the columns (variables) that will
#' be used to summarize, and plot the data frame.
#' @param ... optional arguments that are passed to the function for later use.
#' @param printSummary
#'
#' @return a ggplot2 object with the biplot of the dataframe. Also prints the
#' summary of the data frame.
#'
#' @export
#'
#' @examples

screenFoodWeb <- function (df = NULL, grouping = c("Species", "FG"),
                                printSummary = FALSE, ...){

  summary <- tRophicPosition::summariseIsotopeData(df, grouping, printSummary)

  shape <- as.numeric(factor(summary[[grouping[2]]]))+20

  p <- ggplot(summary, aes(meanC, meanN, fill = factor(summary[[grouping[1]]]))) +
    geom_errorbar(data=summary,aes(ymin=meanN-sdN,ymax=meanN+sdN),width=0.15,
                  color="black", size=.5) +
    geom_errorbarh(data=summary,aes(xmin=meanC-(sdC),xmax=meanC+(sdC)),
                   height=0.15, color="black", size=.5) +
    geom_point(size=4, shape = shape)+
    ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
    xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
    labs(fill = grouping[1])+
    #scale_colour_brewer(RColorBrewer::brewer.pal(length(Strangford.summary$Species_name),"Set1"))+
    #title = "Strangford Lough (mean +- sd)") +
    #geom_point(data=Strangford.summary, size=3, shape = Strangford.summary$FG) +
    #scale_shape_discrete(solid = T) +
    theme_bw()

  #if (!is.null(title)) p <- p + labs(title = title)

  print(p)

}
