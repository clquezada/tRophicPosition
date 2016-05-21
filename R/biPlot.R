#' Function that creates biplot with marginal density functions
#'
#' This function is intended to be used within the package.
#'
#' @param df
#' @param df2
#' @param ylab
#' @param xlab
#' @param p
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

biPlot <- function (df = NULL, df2 = NULL, ylab = NULL, xlab = NULL, p = "p1",
                    ...){

  if (p == "p1") {

    ggplot2::ggplot(df2, ggplot2::aes(mean_d13C, mean_d15N, colour = Factor)) +
      ggplot2::geom_point(data=df2, size=3, shape=1,
                          ggplot2::aes(mean_d13C, mean_d15N)) +
      ggplot2::geom_errorbar(data=df2, ggplot2::aes(ymin=mean_d15N - sd_d15N,
                                          ymax=mean_d15N + sd_d15N), width=0.15) +
      ggplot2::geom_errorbarh(data=df2, ggplot2::aes(xmin=mean_d13C - sd_d13C,
                                           xmax=mean_d13C + sd_d13C, height=0.15)) +
      ggplot2::theme_bw()  +
      ggplot2::geom_point(data=df, ggplot2::aes(d13C,d15N), size=2) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      #ggplot2::scale_colour_brewer(palette = palette, type = type) +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())

  } else if (p == "p2") {

    ggplot2::ggplot(df, ggplot2::aes(x = d13C, colour = Factor, fill = Factor,
                            order = Factor)) +
      ggplot2::geom_density(alpha=0.7) +
      ggplot2::scale_x_continuous(breaks=NULL, expand = c(0,0.1)) +
      ggplot2::scale_y_continuous(breaks=NULL) +
      ggplot2::theme_bw() +
      #ggplot2::scale_colour_brewer(type = "qual", palette = "Set1") +
      theme0(plot.margin = ggplot2::unit(c(0.5,0.2,-0.1,1.5),"cm")) #2,0,-1,2.2 funciona
    #check the margins (ToDo...)

  } else if (p == "p3") {

    ggplot2::ggplot(df, ggplot2::aes(x = d15N, colour = Factor, fill = Factor,
                            order = Factor)) +
      ggplot2::geom_density(alpha=0.7) +
      ggplot2::coord_flip()  +
      ggplot2::scale_x_continuous(breaks=NULL) +
      ggplot2::scale_y_continuous(breaks=NULL, expand = c(0,0)) +
      ggplot2::theme_bw() +
      #ggplot2::scale_colour_brewer(type = type, palette = palette) +
      theme0(plot.margin = ggplot2::unit(c(-0.1,1,1.05,0),"cm")) #0,1,2,-0.5
    #check the margins (ToDo...)

    }

}
