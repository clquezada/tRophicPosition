#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme0 <- function(...) {theme( legend.position = "none",
                                       panel.background = element_blank(),
                                       panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       panel.margin = unit(0,"null"),
                                       axis.ticks = element_blank(),
                                       axis.text.x = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.title.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.ticks.length = unit(0,"null"),
                                       #axis.ticks.margin = unit(0,"null"),
                                       panel.border=element_rect(color=NA),...)}
