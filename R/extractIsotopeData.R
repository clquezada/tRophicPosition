#' Extracts stable isotope data from a data frame
#'
#' @param df data.frame containing raw isotope data, with one or more grouping
#' variables.
#' @param d13C string of the column with d13C isotope values.
#' @param d15N string of the column with d15N isotope values.
#' @param b1 string or vector with the text for baseline 1.
#' @param b2 string or vector with the text for baseline 2.
#' @param baselineColumn string of the column where baselines are grouped.
#' @param speciesColumn string of the column where species/consumers are grouped.
#' @param communityColumn string of the column where communities are grouped.
#' @param deltaC vector of values with trophic discrimination factor for carbon.
#' If NULL it will use Post's assumptions (56 values with 3.4 mean +- 0.98 sd).
#' @param deltaN vector of values with trophic discrimination factor for nitrogen.
#' If NULL it will use Post's assumptions (107 values with 0.39 mean +- 1.3 sd).
#' @param seed integer to get reproducible results
#'
#' @return a list with isotopeData class objects
#' @export
#'
#' @examples
#' data("Bilagay")
#' head(Bilagay)
#' isotopeList <- extractIsotopeData(Bilagay, b1 = "Benthic_BL",
#' b2 = "Pelagic_BL", baselineColumn = "FG", speciesColumn = "Spp",
#' communityColumn = "Location", d13C = "d13C", d15N = "d15N")

extractIsotopeData <- function(df = NULL,
                               b1 = "Baseline 1", b2 = NULL,
                               baselineColumn = "FG",
                               speciesColumn = "Spp",
                               communityColumn = NULL,
                               deltaC = NULL, deltaN = NULL,
                               d13C = "d13C", d15N = "d15N",
                               seed = 666) {

  # extractIsotopeData: no visible binding for global variable ‘species’ fix
  # Check this
  species = NULL


  getValues <- function(df, item, column, isotope)
    df[df[,column] %in% item, isotope]

  getBaselines <- function(df, b1, b2, d15N, d13C) {

    dNb1 <- getValues(df, b1, baselineColumn, d15N)
    dCb1 <- getValues(df, b1, baselineColumn, d13C)

    if (!is.null(b2)) {
      dNb2 <- getValues(df, b2, baselineColumn, d15N)
      dCb2 <- getValues(df, b2, baselineColumn, d13C)

      return(list("dNb1" = dNb1, "dCb1" = dCb1, "dNb2" = dNb2, "dCb2" = dCb2))

    } else

      return(list("dNb1" = dNb1, "dCb1" = dCb1))
    }

  getIsotopeData <- function(df, deltaN, deltaC, speciesColumn, species,
                             community = NULL) {

    siDataList <- list()

    extracted <- getBaselines(df, b1, b2, d15N, d13C)
    extracted[["deltaN"]] <- deltaN
    extracted[["deltaC"]] <- deltaC

    attrb1 <- b1[b1 %in% unique(df[[baselineColumn]])]
    if (!is.null(b2)) attrb2 <- b2[b2 %in% unique(df[[baselineColumn]])]
    else attrb2 = NULL

    df <- df[!df[,baselineColumn] %in% c(b1, b2),]

    for (species in unique(df[[speciesColumn]])) {

      dNc <- getValues(df, species, speciesColumn, d15N)
      dCc <- getValues(df, species, speciesColumn, d13C)

      if (!is.null(community)) species_community <- paste(community,
                                                          species, sep = "-")
      else species_community <- species


      data <- append(extracted, list("dNc" = dNc, "dCc" = dCc))

      siDataList[[species_community]] <- data

      attributes <- list(class = "isotopeData",
                         names = names(data),
                         consumer = species,
                         baseline1 = attrb1,
                         baseline2 = attrb2,
                         community = community)

      mostattributes(siDataList[[species_community]]) <- attributes
    }

    return(siDataList)

  }

  if (is.null(deltaN)) {
    deltaN <- suppressMessages(tRophicPosition::TDF(author = "Post",
                                                    type = "muscle",
                                                    element = "N",
                                                    seed = seed))
    }

  if (is.null(deltaC)) {
    set.seed(seed)
    deltaC <- suppressMessages(tRophicPosition::TDF(author = "Post",
                                                    type = "muscle",
                                                    element = "C",
                                                    seed = seed))
    }

  siDataList <- list()

  if (!is.null(communityColumn)) {

    for (community in unique(df[[communityColumn]])) {

      subset_df <- subset(df, df[,communityColumn] %in% community)

      siDataList <- append(siDataList, getIsotopeData(subset_df,
                                                      deltaN, deltaC,
                                                      speciesColumn, species,
                                                      community))

      }

  } else {

    siDataList <- append(siDataList, getIsotopeData(df,
                                                    deltaN, deltaC,
                                                    speciesColumn, species))

  }

  return(siDataList)

}
