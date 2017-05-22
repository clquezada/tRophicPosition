#' Data frame containing stable isotope values of Bilagay.
#'
#' A dataset containing stable isotope values (d13C and d15N) for the bilagay
#' *Cheilodactylus variegatus*
#' (http://www.fishbase.se/summary/Cheilodactylus-variegatus.html), a fish
#' common to the coastal kelp forests of N Chile.
#'
#' @format A data frame with 841 rows and 7 variables:
#' \describe{
#'   \item{Study}{factor, character describing which study funded data
#'   collection}
#'   \item{Location}{factor, character describing where samples were taken}
#'   \item{Spp}{factor, character describing which scientific name of species}
#'   \item{FG}{factor, character describing functional group of species}
#'   \item{d13C}{numeric, stable isotope d13C values}
#'   \item{d15N}{numeric, stable isotope d15N values}
#'   \item{NS}{numeric, integer describing north to sout ordering (1-10)}
#' }
#'
#' @usage data("Bilagay")
"Bilagay"
