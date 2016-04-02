#' Title
#'
#' @param isotopeData
#'
#' @return
#' @export
#'
#' @examples
#'
#' @note
#'

screenIsotopeData2sources <- function (isotopeData = NULL) {

  #library(RColorBrewer)
  library(ggplot2)
  library(gridExtra)
  #library(ggExtra)

  # Since this function is called from inside the package, we already checked
  # that IsotopeData is not null. But still we check this in case we decide the
  # user can use this function from outside.
  if (!is.null(isotopeData)){

    #First we create a dataframe, reordering the IsoData
    df <- data.frame(d13C = isotopeData$dCsc,
                     d15N =isotopeData$ dNsc,
                     Factor = rep("Secondary consumer",
                                  length(isotopeData$dCsc)))

    df <- rbind (df, data.frame(d13C = isotopeData$dCb1,
                                d15N = isotopeData$dNb1,
                                Factor = rep("Baseline 1",
                                             length(isotopeData$dCb1))))

    df <- rbind (df, data.frame(d13C = isotopeData$dCb2,
                                d15N = isotopeData$dNb2,
                                Factor = rep("Baseline 2",
                                             length(isotopeData$dCb2))))

    #And now we calculate mean and standard deviation for baselines and consumer
    df.b1 <- df[which(df$Factor=="Baseline 1"),1:2]
    a <- lapply(df.b1, mean)
    names(a) <- c("mean_d13C", "mean_d15N")
    b <- lapply(df.b1, sd)
    names(b) <- c("sd_d13C", "sd_d15N")
    b1.meansSDs <- data.frame(a, b, Factor="Baseline 1")

    df.b2 <- df[which(df$Factor=="Baseline 2"),1:2]
    a <- lapply(df.b2, mean)
    names(a) <- c("mean_d13C", "mean_d15N")
    b <- lapply(df.b2, sd)
    names(b) <- c("sd_d13C", "sd_d15N")
    b2.meansSDs <- data.frame(a, b, Factor="Baseline 2")

    df.sc <- df[which(df$Factor=="Secondary consumer"),1:2]
    a <- lapply(df.sc, mean)
    names(a) <- c("mean_d13C", "mean_d15N")
    b <- lapply(df.sc, sd)
    names(b) <- c("sd_d13C", "sd_d15N")
    sc.meansSDs <- data.frame(a, b, Factor="Secondary consumer")

    df2 <- rbind(sc.meansSDs, b1.meansSDs,b2.meansSDs)

    #Here we set up the title and x and y labels (in case that in the future we
    #want to modify through the user. Same applies for the colors)
    xlab <- expression(paste(delta^{13}, "C (\u2030)"))
    ylab <- expression(paste(delta^{15}, "N (\u2030)"))
    #title="Title ñe"
    colors <- RColorBrewer::brewer.pal(3, "Set1")

    #This is working
#     p1 <- ggplot(df2, aes(mean_d13C, mean_d15N, colour = Factor)) +
#       geom_point(data=df2, size=3, shape=1, aes(mean_d13C, mean_d15N)) +
#       geom_errorbar(data=df2,aes(ymin=mean_d15N - sd_d15N,
#                                  ymax=mean_d15N + sd_d15N),width=0.15) +
#       geom_errorbarh(data=df2,aes(xmin=mean_d13C - sd_d13C,
#                                   xmax=mean_d13C + sd_d13C,height=0.15)) +
#       theme_bw()  + geom_point(data=df, aes(d13C,d15N), size=2) +
#       ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
#       xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
#       scale_colour_manual(values=colors, name="Legend")
#
#     p1

    #not working ggMarginal(p1)
    #now working?
    #ggExtra?
    #ggMarginal(p1)

    #This is to plot only the Secondary consumer data
    #df <- df[which(df$Factor=="Secondary consumer"),]

    #This is working
    p1 <- ggplot(df2, aes(mean_d13C, mean_d15N, colour = Factor)) +
      geom_point(data=df2, size=3, shape=1, aes(mean_d13C, mean_d15N)) +
      geom_errorbar(data=df2,aes(ymin=mean_d15N - sd_d15N,
                                 ymax=mean_d15N + sd_d15N), width=0.15) +
      geom_errorbarh(data=df2,aes(xmin=mean_d13C - sd_d13C,
                                  xmax=mean_d13C + sd_d13C, height=0.15)) +
      theme_bw()  + geom_point(data=df, aes(d13C,d15N), size=2) +
      ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
      xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
      theme(legend.position = "none")
      #scale_colour_manual(values=colors, name="Legend")

    p2 <- ggplot(df, aes(x = d13C, colour = Factor, fill = Factor)) +
      geom_density(alpha=0.7) +
      scale_x_continuous(breaks=NULL,expand=c(0.02,0)) +
      scale_y_continuous(breaks=NULL,expand=c(0.02,0)) +
      theme_bw() +
      theme0(plot.margin = unit(c(2,0,0.05,2.2),"lines")) #2,0,-1,2.2 funciona

    p3 <- ggplot(df, aes(x = d15N, colour = Factor, fill = Factor)) +
      geom_density(alpha=0.7) +
      coord_flip()  +
      scale_x_continuous(labels = NULL,breaks=NULL,expand=c(0.02,0)) +
      scale_y_continuous(labels = NULL,breaks=NULL,expand=c(0.02,0)) +
      theme_bw() +
      theme0(plot.margin = unit(c(0,1,2,-0.1),"lines"))

    grid.arrange(arrangeGrob(p2,ncol=2,widths=c(3,1)),
                 arrangeGrob(p1,p3,ncol=2,widths=c(3,1)),
                 heights=c(1,3))

    }

}
