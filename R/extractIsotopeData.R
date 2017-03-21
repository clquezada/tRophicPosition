#' Title
#'
#'
#' @param df
#' @param d13C
#' @param d15N
#' @param b1
#' @param b2
#' @param baselineColumn
#' @param speciesColumn
#' @param communityColumn
#' @param deltaC
#' @param deltaN
#'
#' @return
#' @export
#'
#' @examples

extractIsotopeData <- function(df = NULL,
                               b1 = "Baseline 1", b2 = NULL,
                               baselineColumn = "FG",
                               speciesColumn = "Spp",
                               communityColumn = NULL,
                               deltaC = NULL, deltaN = NULL,
                               d13C = "d13C", d15N = "d15N",
                               seed = 666) {

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

      if (!is.null(community)) species_community <- paste(community, species, sep = "-")
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
    deltaN <- suppressMessages(tRophicPosition::TEF(author = "McCutchan",
                                                    type = "muscle",
                                                    element = "N"))
      #simulateTEF(meanN = 3.4, sdN = 0.98)
    }

  if (is.null(deltaC)) {
    set.seed(seed)
    deltaC <- suppressMessages(tRophicPosition::TEF(author = "McCutchan",
                                                    type = "muscle",
                                                    element = "C"))
      #simulateTEF(meanC = 0.39, sdC = 1.3)
    }

  siDataList <- list()

  if (!is.null(communityColumn)) {

    for (community in unique(df[[communityColumn]])) {

      # extracted <- list()

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
