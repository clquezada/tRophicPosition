#' Trophic discrimination factors from bibliography
#'
#' This function returns trophic discrimination factors (TDF), given an author,
#' element and a type. For convenience 'type' includes a number of categories
#' depending on the author. At the moment it includes TDF data from Post (2002)
#' and from McCutchan et al (2003).
#'
#' @param author could be either "Post" or "McCutchan" at the moment.
#' @param element can be "both", "N" or "C"
#' @param type this argument only works for "McCutchan" author (their Table 3).
#'   "all" returns all TDF data; "whole" and "muscle" returns TDF separated per
#'   type analysis; "acidified" and "unacidified" returns TDF separated per
#'   acidification; and "Rainbow Trout" and "Brook Trout" returns TDF separated
#'   per fish species (according to their Table 1).
#' @param seed integer to have replicated results
#'
#' @return a list (if element = "both") or a vector (if element ="N" or element
#'   = "C") containing TDF values
#' @export
#'
#' @examples TDF(author = "McCutchan", element = "N")
#'
TDF <- function (author = "Post", element = "both", type = "all", seed = 3) {

  meanSD <- function(x, mean, sd) {

    set.seed(seed = seed)
    x <- stats::rnorm(x, mean, sd)
    X <- x
    MEAN <- mean
    SD <- sd
    Z <- (((X - mean(X, na.rm = TRUE))/sd(X, na.rm = TRUE))) * SD
    MEAN + Z
  }

  meanSE <- function(x, mean, se) {

    sd <- se * sqrt(x)
    meanSD(x, mean, sd)
  }

  if (author == "Post") {

    set.seed(seed = seed)
    deltaN = meanSD(56, 3.4, 0.98)
    set.seed(seed = seed)
    deltaC = meanSD(107, 0.39, 1.3)

    if (element == "N") {
      message("You selected Post's (2002)
              d15N: 56 values with 3.4 mean +- 0.98 sd")
      return(deltaN)

    } else if (element == "C") {

      message("You selected Post's (2002)
              d13C: 107 values with 0.39 mean +- 1.3 sd")
      return(deltaC)

    } else if (element == "both") {

      message("You selected Post's (2002)
              d15N: 56 values with 3.4 mean +- 0.98 sd")
      message("d13C: 107 values with 0.39 mean +- 1.3 sd")
      return(list(deltaN = deltaN, deltaC = deltaC))

    } } else if (author == "McCutchan") {

      set.seed(seed = seed)
      allN = meanSE(73, 2.3, 0.18)
      set.seed(seed = seed)
      wholeN = meanSE(58, 2.1, 0.21)
      set.seed(seed = seed)
      muscleN = meanSE(15, 2.9, 0.32)
      set.seed(seed = seed)
      unacidifiedN = meanSE(15, 1.1, 0.29)
      set.seed(seed = seed)
      acidifiedN = meanSE(36, 2.4, 0.24)
      #raisedInvN = meanSE(25, 1.4, 0.21)
      #highProtN = meanSE(25, 3.3, 0.26)
      #plantAlgaeN = meanSE(25, 2.2, 0.3)
      set.seed(seed = seed)
      rainbowTroutN = meanSE(4, 3.2, 0.2)
      set.seed(seed = seed)
      brookTroutN = meanSE(8, 3.8, 0.17)

      set.seed(seed = seed)
      allC = meanSE(102, 0.5, 0.13)
      set.seed(seed = seed)
      wholeC = meanSE(84, 0.3, 0.14)
      set.seed(seed = seed)
      muscleC = meanSE(18, 1.3, 0.3)
      set.seed(seed = seed)
      unacidifiedC = meanSE(62, 0.5, 0.17)
      set.seed(seed = seed)
      acidifiedC = meanSE(22, -0.2, 0.21)
      set.seed(seed = seed)
      rainbowTroutC = meanSE(4, 1.9, 0.51)
      set.seed(seed = seed)
      brookTroutC = meanSE(8, 3.3, 0.29)

      if (type == "all") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  All d15N: 73 values with 2.3 mean +- 0.18 se")
          return(allN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  All d13C: 102 values with 0.5 mean +- 0.13 se")
          return(allC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  All d15N: 73 values with 2.3 mean +- 0.18 se")
          message("All d13C: 102 values with 0.5 mean +- 0.13 se")
          return(list(deltaN = allN, deltaC = allC))
        }

      } else if (type == "whole") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  Whole tissue d15N: 58 values with 2.1 mean +- 0.21 se")
          return(wholeN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  Whole tissue d13C: 84 values with 0.3 mean +- 0.14 se")
          return(wholeC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  Whole tissue d15N: 58 values with 2.1 mean +- 0.21 se")
          message("Whole tissue d13C: 84 values with 0.3 mean +- 0.14 se")
          return(list(deltaN = wholeN, deltaC = wholeC))
        }

      } else if (type == "muscle") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  Muscle tissue d15N: 15 values with 2.9 mean +- 0.32 se")
          return(muscleN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  Muscle tissue d13C: 18 values with 1.3 mean +- 0.3 se")
          return(muscleC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  Muscle tissue d15N: 15 values with 2.9 mean +- 0.32 se")
          message("Muscle tissue d13C: 18 values with 1.3 mean +- 0.3 se")
          return(list(deltaN = muscleN, deltaC = muscleC))
        }

      } else if (type == "unacidified") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  Unacidified tissue d15N: 15 values with 1.1 mean +- 0.29 se")
          return(unacidifiedN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  Unacidified tissue d13C: 62 values with 0.5 mean +- 0.17 se")
          return(unacidifiedC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  Unacidified tissue d15N: 15 values with 1.1 mean +- 0.29 se")
          message("Unacidified tissue d13C: 62 values with 0.5 mean +- 0.17 se")
          return(list(deltaN = unacidifiedN, deltaC = unacidifiedC))
        }

      } else if (type == "acidified") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  Acidified tissue d13C: 22 values with -0.2 mean +- 0.21 se")
          return(acidifiedN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  Acidified tissue d15N: 36 values with 2.4 mean +- 0.24 se")
          return(acidifiedC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  Acidified tissue d15N: 36 values with 2.4 mean +- 0.24 se")
          message("Acidified tissue d13C: 22 values with -0.2 mean +- 0.21 se")
          return(list(deltaN = acidifiedN, deltaC = acidifiedC))
        }

      } else if (type == "rainbowTrout") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  Rainbow Trout tissue d15N: 4 values with 3.2 mean +- 0.2 se")
          return(rainbowTroutN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  Muscle tissue d13C: 4 values with 1.9 mean +- 0.51 se")
          return(rainbowTroutC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  Rainbow Trout tissue d15N: 4 values with 3.2 mean +- 0.2 se")
          message("Rainbow Trout muscle tissue d13C: 4 values with 1.9 mean +- 0.51 se")
          return(list(deltaN = rainbowTroutN, deltaC = rainbowTroutC))
        }

      } else if (type == "brookTrout") {

        if (element == "N") {

          message("You selected McCutchan's et al (2003)
                  Brook Trout tissue d13C: 8 values with 3.3 mean +- 0.29 se")
          return(brookTroutN)

        } else if (element == "C") {

          message("You selected McCutchan's et al (2003)
                  Brook Trout tissue d15N: 8 values with 3.8 mean +- 0.17 se")
          return(brookTroutC)

        } else if (element == "both") {

          message("You selected McCutchan's et al (2003)
                  Brook Trout tissue d15N: 8 values with 3.8 mean +- 0.17 se")
          message("Brook Trout tissue d13C: 8 values with 3.3 mean +- 0.29 se")
          return(list(deltaN = brookTroutN, deltaC = brookTroutC))
        }

      }


 }

}
