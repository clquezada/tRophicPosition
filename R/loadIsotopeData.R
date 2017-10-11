#' Extract and load stable isotope data for selected consumers from a data frame
#'
#' This function extracts only selected consumers/species with their respective
#' baseline(s) and returns an isotopeData class object (or list). It is useful
#' when there are a lot of information in a data frame and you want to calculate
#' trophic position only for selected consumers in one or more communities.
#'
#' @param df data frame containing raw isotope data with at least one grouping
#'   column.
#' @param species string or character vector indicating which consumer/species
#'   will be extracted.
#' @param speciesColumn string of the column where species/consumers are
#'   grouped.
#' @param community string or character vector indicating which community(ies)
#'   will be extracted.
#' @param communityColumn string of the column where communities are grouped.
#' @param b1 string or character vector indicating which baseline(s) will be
#'   extracted as baseline 1.
#' @param b2 string or character vector indicating which baseline(s) will be
#'   extracted as baseline 2.
#' @param baselineColumn string of the column where baselines are grouped.
#' @param d13C string indicating from which column extract d13C isotope values.
#' @param d15N string indicating from which column extract d15N isotope values.
#' @param deltaC vector of values with trophic discrimination factor for carbon.
#'   If NULL it will use Post's assumptions (56 values with 3.4 mean +- 0.98
#'   sd).
#' @param deltaN vector of values with trophic discrimination factor for
#'   nitrogen. If NULL it will use Post's assumptions (107 values with 0.39 mean
#'   +- 1.3 sd).
#' @param seed numerical value to get reproducible results with trophic
#'   discrimination factors (because they are simulated each time this function
#'   is called). By default, is 3.
#'
#' @return an isotopeData class object if one consumer and one community are
#'   selected. A list of isotopeData class objects if more than one consumer or
#'   more than one community are selected.
#' @export
#'
#' @examples
#' data("Bilagay")
#' head(Bilagay)
#' loadIsotopeData(df = Bilagay, species = "Bilagay", speciesColumn = "FG",
#' community = c("CHI", "COL"), communityColumn = "Location",
#' b1 = "Benthic_BL", b2 = "Pelagic_BL", baselineColumn = "FG")

loadIsotopeData <- function(df = NULL,
                            species = NULL,
                            community = NULL,
                            b1 = "Baseline 1", b2 = NULL,
                            baselineColumn = "FG",
                            speciesColumn = "FG",
                            communityColumn = NULL,
                            d13C = "d13C", d15N = "d15N",
                            deltaC = NULL, deltaN = NULL,
                            seed = 666) {

  if (is.null(species)) stop("species can not be NULL")

  getValues <- function(df, item, column)
    df[df[,column] %in% item,]

  new_df <- data.frame(df[0,])

  if(!is.null(community) & !is.null(communityColumn)) {
    for (site in community){
      site_subset <- getValues(df, site, communityColumn)

      species_subset <- getValues(site_subset, species, speciesColumn)

      if (nrow(species_subset) == 0) next
      else new_df <- rbind(new_df, species_subset)
      new_df <- rbind(new_df, getValues(site_subset, b1, baselineColumn))

      if (!is.null(b2)) new_df <- rbind(new_df, getValues(site_subset, b2,
                                                          baselineColumn))
    }

  } else {
    for (sp in species) {
      species_subset <- getValues(df, sp, speciesColumn)

      if (nrow(species_subset) == 0) next
      else new_df <- rbind(new_df, species_subset)
      new_df <- rbind(new_df, getValues(df, b1, baselineColumn))

      if (!is.null(b2)) new_df <- rbind(new_df, getValues(df, b2,
                                                          baselineColumn))
    }
  }

  isotopes <- extractIsotopeData(new_df, b1 = b1, b2 = b2,
                                 baselineColumn = baselineColumn,
                                 speciesColumn = speciesColumn,
                                 communityColumn = communityColumn,
                                 deltaC = deltaC, deltaN = deltaN,
                                 d13C = d13C, d15N = d15N,
                                 seed = seed)

  if(length(isotopes) == 1) return(isotopes[[1]])

  else return(isotopes)

  }
