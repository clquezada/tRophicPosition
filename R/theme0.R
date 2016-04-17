#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme0 <- function(...) {
  ggplot2::theme( legend.position = "none",
         panel.background = ggplot2::element_blank(),
         panel.grid.major = ggplot2::element_blank(),
         panel.grid.minor = ggplot2::element_blank(),
         panel.margin = ggplot2::unit(0,"null"),
         axis.ticks = ggplot2::element_blank(),
         axis.text.x = ggplot2::element_blank(),
         axis.text.y = ggplot2::element_blank(),
         axis.title.x = ggplot2::element_blank(),
         axis.title.y = ggplot2::element_blank(),
         axis.ticks.length = ggplot2::unit(0,"null"),
         #axis.ticks.margin = unit(0,"null"),
         panel.border = ggplot2::element_rect(color=NA),...)}
