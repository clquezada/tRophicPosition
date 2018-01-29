#' Extract and load stable isotope data for selected consumers from a data frame
#'
#' This function extracts only selected consumers/species with their respective
#' baseline(s) and returns an isotopeData class object (or list). It is useful
#' when there are a lot of information in a data frame and you want to calculate
#' trophic position only for selected consumers in one or more communities.
#'
#' @param df data frame containing raw isotope data with at least one grouping
#'   column.
#' @param consumer string or character vector indicating which consumer/species
#'   will be extracted.
#' @param consumersColumn string of the column where species/consumer(s) are
#'   grouped.
#' @param group string or character vector indicating which group(s)
#'   will be extracted.
#' @param groupsColumn string of the column where groups/communities are
#' grouped.
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
#' @param ... Additional arguments passed to this function.
#'
#' @return an isotopeData class object if one consumer and one group are
#'   selected. A list of isotopeData class objects if more than one consumer or
#'   more than one group are selected.
#' @export
#'
#' @examples
#' data("Bilagay")
#' head(Bilagay)
#' loadIsotopeData(df = Bilagay, consumer = "Bilagay", consumersColumn = "FG",
#' group = c("CHI", "COL"), groupsColumn = "Location",
#' b1 = "Benthic_BL", b2 = "Pelagic_BL", baselineColumn = "FG")

loadIsotopeData <- function(df = NULL,
                            consumer = NULL,
                            group = NULL,
                            b1 = "Baseline 1", b2 = NULL,
                            baselineColumn = "FG",
                            consumersColumn = "FG",
                            groupsColumn = NULL,
                            d13C = "d13C", d15N = "d15N",
                            deltaC = NULL, deltaN = NULL,
                            seed = 666,
                            ...) {

  arguments <- list(...) #might be names(as.list(match.call())[-1])

  if("species" %in% names(arguments)){
    consumer <- list(...)$species
    message(strwrap("The Argument 'species' was maintained for compatibility
                    tRophicPosition =< 0.7.5. Avoid to use it in the future and
                    use 'consumersColumn' instead. Check the help for more
                    details."))
  }

  if (is.null(consumer)) stop("consumer can not be NULL")
  if (is.null(consumersColumn)) stop("consumersColumn can not be NULL")



  if("communityColumn" %in% names(arguments)){
    groupsColumn <- list(...)$communityColumn
    message("The Argument 'communityColumn' was maintained for compatibility
            with tRophicPosition =< 0.7.5. Avoid to use it in the future and
            use 'groupsColumn' instead. Check the help for more details.")
  }

  for (column in c(baselineColumn, consumersColumn, groupsColumn,
                   d13C, d15N))
    if(!(column %in% names(df)))
      stop(paste0("The column ", column,
                  " is not present in your data frame"))

  getValues <- function(df, item, column)
    df[df[,column] %in% item,]

  new_df <- data.frame(df[0,])

  if(!is.null(group) & !is.null(groupsColumn)) {
    for (site in group){
      site_subset <- getValues(df, site, groupsColumn)

      consumer_subset <- getValues(site_subset, consumer, consumersColumn)

      if (nrow(consumer_subset) == 0) next
      else new_df <- rbind(new_df, consumer_subset)
      new_df <- rbind(new_df, getValues(site_subset, b1, baselineColumn))

      if (!is.null(b2)) new_df <- rbind(new_df, getValues(site_subset, b2,
                                                          baselineColumn))
    }

  } else {
    for (sp in consumer) {
      consumer_subset <- getValues(df, sp, consumersColumn)

      if (nrow(consumer_subset) == 0) next
      else new_df <- rbind(new_df, consumer_subset)
      new_df <- rbind(new_df, getValues(df, b1, baselineColumn))

      if (!is.null(b2)) new_df <- rbind(new_df, getValues(df, b2,
                                                          baselineColumn))
    }
  }

  isotopes <- extractIsotopeData(new_df, b1 = b1, b2 = b2,
                                 baselineColumn = baselineColumn,
                                 consumersColumn = consumersColumn,
                                 groupsColumn = groupsColumn,
                                 deltaC = deltaC, deltaN = deltaN,
                                 d13C = d13C, d15N = d15N,
                                 seed = seed)

  if(length(isotopes) == 1) return(isotopes[[1]])

  else return(isotopes)

  }
