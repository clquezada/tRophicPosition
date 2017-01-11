#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

credibilityIntervals <- function(df, x, y, lower, upper, ...) {

  ggplot(df, aes(x = x, y = y, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 1, shape = 21, fill = "grey50", colour = "grey50") +
    theme_bw() +
    ylab("Posterior Trophic Position") + xlab("Site") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title = element_text(size = 14))

}
