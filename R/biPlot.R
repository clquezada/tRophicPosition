# Internal functions within tRophicPosition
#
# These functions are intended to be used within the package, and not directly by the users
#
# @param df dataframe 1
# @param df2 dataframe 2
# @param ylab ylab
# @param xlab xlab
# @param p p
# @param legend legend
# @param limits limits
# @param ... additional arguments passed to this function.
#
# @return Multiple objects returned
#
biPlot <- function (df = NULL, df2 = NULL, ylab = NULL, xlab = NULL, p = "p1",
                    legend = legend, limits = limits, ...){

  # Stupid CRAN fix for variables - see here http://stackoverflow.com/questions/
  # 9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable
  # -notes-when
  # As seen in https://github.com/andrewcparnell/simmr/blob/master/R/plot.simmr
  # _output.R
  mean_d13C <- mean_d15N <- Factor <- sd_d15N <- sd_d13C <- d13C <- d15N <- NULL

  if (p == "p1") {

     pNew <- ggplot2::ggplot(df2, ggplot2::aes(mean_d13C, mean_d15N,
                                               colour = Factor)) +
      ggplot2::geom_point(data=df2, size=3, shape=1,
                          ggplot2::aes(mean_d13C, mean_d15N)) +
      ggplot2::geom_errorbar(data=df2, ggplot2::aes(ymin=mean_d15N -
                                                      sd_d15N,
                                          ymax=mean_d15N + sd_d15N),
                             width=0.15) +
      ggplot2::geom_errorbarh(data=df2, ggplot2::aes(xmin=mean_d13C -
                                                       sd_d13C,
                                           xmax=mean_d13C + sd_d13C,
                                           height=0.15)) +
      ggplot2::scale_colour_manual(values = c("#2b83ba", "#abdda4", "#d7191c")) +
      ggplot2::theme_bw()  +
      ggplot2::geom_point(data=df, ggplot2::aes(d13C,d15N), size=2) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab)
      #ggplot2::scale_colour_brewer(palette = palette, type = type)

     if (is.numeric(legend) & length(legend) == 2 & !is.null(legend)) {
       pNew <- pNew + ggplot2::theme(legend.position = legend,
                             legend.title = ggplot2::element_blank(),
                             legend.background =
                               ggplot2::element_rect(fill="transparent"))
     }


     # if(!is.null(title)) pNew <- pNew + ggplot2::ggtitle(title)

     list(ggplot2::ggplot_build(pNew)$layout$panel_ranges[[1]]$x.range,
          ggplot2::ggplot_build(pNew)$layout$panel_ranges[[1]]$y.range,
          pNew)

  } else if (p == "p2") {

    ggplot2::ggplot(df, ggplot2::aes(x = d13C, colour = Factor, fill = Factor,
                            order = Factor)) +
      ggplot2::geom_density(alpha=0.7) +
      ggplot2::scale_x_continuous(breaks=NULL, expand = c(0.015,0),
                                  limits = limits) +
      ggplot2::scale_y_continuous(breaks=NULL) +
      ggplot2::scale_colour_manual(values = c("#2b83ba", "#abdda4", "#d7191c")) +
      ggplot2::scale_fill_manual(values = c("#2b83ba", "#abdda4", "#d7191c")) +
      ggplot2::theme_bw() +
      #ggplot2::scale_colour_brewer(type = "qual", palette = "Set1") +
      theme0(plot.margin =
               ggplot2::unit(c(0.45,0,-0.1,1.6),"cm")) #2,0,-1,2.2 funciona
    #check the margins (ToDo...)

  } else if (p == "p3") {

    ggplot2::ggplot(df, ggplot2::aes(x = d15N, colour = Factor, fill = Factor,
                            order = Factor)) +
      ggplot2::geom_density(alpha=0.7) +
      ggplot2::coord_flip()  +
      ggplot2::scale_x_continuous(breaks=NULL, limits = limits) +
      ggplot2::scale_y_continuous(breaks=NULL, expand = c(0,0)) +
      ggplot2::scale_colour_manual(values = c("#2b83ba", "#abdda4", "#d7191c")) +
      ggplot2::scale_fill_manual(values = c("#2b83ba", "#abdda4", "#d7191c")) +
      ggplot2::theme_bw() +
      #ggplot2::scale_colour_brewer(type = type, palette = palette) +
      theme0(plot.margin = ggplot2::unit(c(-0.1,1,1.2,0),"cm")) #0,1,2,-0.5
    #check the margins (ToDo...)

    }

}
