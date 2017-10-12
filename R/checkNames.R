# Internal function that check the names of a list.
#
# Not intended to be used by the user.
#
# @param df named list with the variables to be checked for.
# @param flag integer (stating the number of baselines) or a list of names
# (stating the names expected).
#
checkNames <- function (df = NULL, flag = NULL) {

  if (typeof(flag) == "double") {

    if (flag == 1) {
      namesDF <- c("dNb1", "dCb1", "dNc", "dCc")

    } else if (flag == 2) {
      namesDF <- c("dNb1", "dNb2", "dCb1", "dCb2", "dNc", "dCc")

    } else if (flag == 3) {
      namesDF <- c("dNb1", "dNb2", "dNb3", "dCb1", "dCb2", "dCb3", "dNc",
                   "dCc")
    } else if (flag == 4) {
      namesDF <- c("dNb1", "dCb1", "dNb2", "dCb2", "dNc", "dCc", "deltaN",
                   "deltaC")
    }

  } else if (typeof(flag) == "character") {

    namesDF <- flag

  } else if (is.null(flag)) {
    message("Flag is NULL")

    } else {

    stop("Flag must be either 1, 2, 3 or a list of names. Check the argument 'flag'.")
  }

  for (name in namesDF) {
    counter <- 0

    if ((name %in% names(df)) == FALSE) {
      counter <- counter + 1
    }
  }

  if (counter > 0) {
    message("Names of your dataframe: ", paste0(names(df), sep = " "), "\n")
    message("Names expected: ", paste0(unlist(namesDF), sep = " "), "\n")
    message("Flag: ", paste0(unlist(flag), sep = " "), "\n")
    message("You have at least one variable in your dataframe that does not
match the names expected.")
    return(NULL)
  }

  return(TRUE)
}

