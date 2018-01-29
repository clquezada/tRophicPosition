#' Extract stable isotope data from a data frame
#'
#' This function generates a list of isotopeData class objects parsing a data
#' frame of stable isotope values analysed for one or more consumers and one or
#' two baselines. The data frame can be organized in one or more communities (or
#' sampling sites, samples in time, multiple studies, etc.).
#'
#' @param df data frame containing raw isotope data, with one or more grouping
#'   variables.
#' @param d13C string of the column that has d13C isotope values.
#' @param d15N string of the column that has d15N isotope values.
#' @param b1 string or vector with the text for baseline 1.
#' @param b2 string or vector with the text for baseline 2.
#' @param baselineColumn string of the column where baselines are grouped.
#' @param consumersColumn string of the column where consumers/species are
#'   grouped.
#' @param groupsColumn string of the column where groups/communities are grouped.
#' @param deltaC vector of values with trophic discrimination factor for carbon.
#'   If NULL it will use Post's assumptions (56 values with 3.4 mean +- 0.98
#'   sd).
#' @param deltaN vector of values with trophic discrimination factor for
#'   nitrogen. If NULL it will use Post's assumptions (107 values with 0.39 mean
#'   +- 1.3 sd).
#' @param seed integer to get reproducible results. By default, seed = 3.
#' @param ... Additional arguments passed to this funcion.
#'
#' @return a list with isotopeData class objects
#' @export
#'
#' @examples
#' data("Bilagay")
#' head(Bilagay)
#' isotopeList <- extractIsotopeData(Bilagay, b1 = "Benthic_BL",
#' b2 = "Pelagic_BL", baselineColumn = "FG", consumersColumn = "Spp",
#' groupsColumn = "Location", d13C = "d13C", d15N = "d15N")

extractIsotopeData <- function(df = NULL,
                               b1 = "Baseline 1", b2 = NULL,
                               baselineColumn = "FG",
                               consumersColumn = "Spp",
                               groupsColumn = NULL,
                               deltaC = NULL, deltaN = NULL,
                               d13C = "d13C", d15N = "d15N",
                               seed = 3,
                               ...) {

  # extractIsotopeData: no visible binding for global variable species fix
  # Check this
  # species = NULL

  arguments <- list(...) #might be names(as.list(match.call())[-1])

  if("speciesColumn" %in% names(arguments)){
    consumersColumn <- list(...)$speciesColumn
    message("The Argument 'speciesColum' is maintained for compatibility with
        tRophicPosition =< 0.7.5. Avoid to use it in the future and use
        'consumersColumn' instead. Check the help for more details.")
  }

  if("communityColumn" %in% names(arguments)){
    groupsColumn <- list(...)$communityColumn
    message("The Argument 'communityColumn' is maintained for compatibility with
            tRophicPosition =< 0.7.5. Avoid to use it in the future and use
            'groupsColumn' instead. Check the help for more details.")
  }

  for (column in c(baselineColumn, consumersColumn, groupsColumn,
                   d13C, d15N))
    if(!(column %in% names(df)))
      stop(paste0("The column ", column,
                  " is not present in your data frame"))

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

  getIsotopeData <- function(df, deltaN, deltaC, consumersColumn,
                             group = NULL) {

    siDataList <- list()

    extracted <- getBaselines(df, b1, b2, d15N, d13C)
    extracted[["deltaN"]] <- deltaN
    extracted[["deltaC"]] <- deltaC

    attrb1 <- b1[b1 %in% unique(df[[baselineColumn]])]
    if (!is.null(b2)) attrb2 <- b2[b2 %in% unique(df[[baselineColumn]])]
    else attrb2 = NULL

    df <- df[!df[,baselineColumn] %in% c(b1, b2),]

    for (consumer in unique(df[[consumersColumn]])) {

      dNc <- getValues(df, consumer, consumersColumn, d15N)
      dCc <- getValues(df, consumer, consumersColumn, d13C)

      if (!is.null(group)) consumer_group <- paste(group,
                                                          consumer, sep = "-")
      else consumer_group <- consumer


      data <- append(extracted, list("dNc" = dNc, "dCc" = dCc))

      siDataList[[consumer_group]] <- data

      attributes <- list(class = "isotopeData",
                         names = names(data),
                         consumer = consumer,
                         baseline1 = attrb1,
                         baseline2 = attrb2,
                         group = group)

      mostattributes(siDataList[[consumer_group]]) <- attributes
    }

    return(siDataList)

  }

  if (is.null(deltaN)) {
    deltaN <- suppressMessages(tRophicPosition::TDF(author = "Post",
                                                    #type = "muscle",
                                                    element = "N",
                                                    seed = seed))
    }

  if (is.null(deltaC)) {
    set.seed(seed)
    deltaC <- suppressMessages(tRophicPosition::TDF(author = "Post",
                                                    #type = "muscle",
                                                    element = "C",
                                                    seed = seed))
    }

  siDataList <- list()

  if (!is.null(groupsColumn)) {

    for (group in unique(df[[groupsColumn]])) {

      subset_df <- subset(df, df[,groupsColumn] %in% group)

      siDataList <- append(siDataList, getIsotopeData(subset_df,
                                                      deltaN, deltaC,
                                                      consumersColumn,
                                                      group))

      }

  } else {

    siDataList <- append(siDataList, getIsotopeData(df,
                                                    deltaN, deltaC,
                                                    consumersColumn))

  }

  return(siDataList)

}
