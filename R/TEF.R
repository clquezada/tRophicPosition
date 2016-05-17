#' Title
#'
#' @param author
#' @param element
#'
#' @return
#' @export
#'
#' @examples
#'
TEF <- function (author = "Post", element = "both", type = "all") {

  meanSD <- function(x, mean, sd) {

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

    deltaN = meanSD(56, 3.4, 0.98)
    deltaC = meanSD(107, 0.39, 1.3)

    if (element == "N") {
      message("You select Post's (2002) d15N: 56 values with 3.4 mean +- 0.98 sd")
      return(deltaN)

    } else if (element == "C") {

      message("You select Post's (2002) d13C: 107 values with 0.39 mean +- 1.3 sd")
      return(deltaC)

    } else if (element == "both") {

      message("You select Post's (2002) d15N: 56 values with 3.4 mean +- 0.98 sd")
      message("and also Post's (2002) d13C: 107 values with 0.39 mean +- 1.3 sd")
      return(list(deltaN = deltaN, deltaC = deltaC))

    } } else if (author == "McCutchan") {

      allN = meanSE(73, 2.3, 0.18)
      wholeN = allN
      muscleN = wholeN
      unacidifiedN = meanSE(15, 1.1, 0.29)
      acidifiedN = meanSE(36, 2.4, 0.24)
      #raisedInvN = meanSE(25, 1.4, 0.21)
      #highProtN = meanSE(25, 3.3, 0.26)
      #plantAlgaeN = meanSE(25, 2.2, 0.3)
      rainbowTroutN = meanSE(4, 3.2, 0.2)
      brookTroutN = meanSE(8, 3.8, 0.17)

      allC = meanSE(102, 0.5, 0.13)
      wholeC = meanSE(84, 0.3, 0.14)
      muscleC = meanSE(18, 1.3, 0.3)
      unacidifiedC = meanSE(62, 0.5, 0.17)
      acidifiedC = meanSE(22, -0.2, 0.21)
      rainbowTroutC = meanSE(4, 1.9, 0.51)
      brookTroutC = meanSE(8, 3.3, 0.29)

      if (type == "all") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) all d15N: 73 values with 2.3 mean +- 0.18 se")
          return(allN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) all d13C: 102 values with 0.5 mean +- 0.13 se")
          return(allC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) all d15N: 73 values with 2.3 mean +- 0.18 se")
          message("and also McCutchan's et al (2003) all d13C: 102 values with 0.5 mean +- 0.13 se")
          return(list(deltaN = allN, deltaC = allC))
        }

      } else if (type == "whole") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) whole tissue d15N: 73 values with 2.3 mean +- 0.18 se")
          return(wholeN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) whole tissue d13C: 84 values with 0.3 mean +- 0.14 se")
          return(wholeC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) whole tissue d15N: 73 values with 2.3 mean +- 0.18 se")
          message("and also McCutchan's et al (2003) whole tissue d13C: 84 values with 0.3 mean +- 0.14 se")
          return(list(deltaN = wholeN, deltaC = wholeC))
        }

      } else if (type == "muscle") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) muscle tissue d15N: 73 values with 2.3 mean +- 0.18 se")
          return(muscleN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) muscle tissue d13C: 18 values with 1.3 mean +- 0.3 se")
          return(muscleC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) muscle tissue d15N: 73 values with 2.3 mean +- 0.18 se")
          message("and also McCutchan's et al (2003) muscle tissue d13C: 18 values with 1.3 mean +- 0.3 se")
          return(list(deltaN = muscleN, deltaC = muscleC))
        }

      } else if (type == "unacidified") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) unacidified tissue d15N: 15 values with 1.1 mean +- 0.29 se")
          return(unacidifiedN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) unacidified tissue d13C: 62 values with 0.5 mean +- 0.17 se")
          return(unacidifiedC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) unacidified tissue d15N: 15 values with 1.1 mean +- 0.29 se")
          message("and also McCutchan's et al (2003) unacidified tissue d13C: 62 values with 0.5 mean +- 0.17 se")
          return(list(deltaN = unacidifiedN, deltaC = unacidifiedC))
        }

      } else if (type == "acidified") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) acidified tissue d13C: 22 values with -0.2 mean +- 0.21 se")
          return(acidifiedN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) acidified tissue d15N: 36 values with 2.4 mean +- 0.24 se")
          return(acidifiedC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) acidified tissue d15N: 36 values with 2.4 mean +- 0.24 se")
          message("and also McCutchan's et al (2003) acidified tissue d13C: 22 values with -0.2 mean +- 0.21 se")
          return(list(deltaN = acidifiedN, deltaC = acidifiedC))
        }

      } else if (type == "rainbowTrout") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) Rainbow Trout tissue d15N: 4 values with 3.2 mean +- 0.2 se")
          return(rainbowTroutN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) muscle tissue d13C: 4 values with 1.9 mean +- 0.51 se")
          return(rainbowTroutC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) Rainbow Trout tissue d15N: 4 values with 3.2 mean +- 0.2 se")
          message("and also McCutchan's et al (2003) muscle tissue d13C: 4 values with 1.9 mean +- 0.51 se")
          return(list(deltaN = rainbowTroutN, deltaC = rainbowTroutC))
        }

      } else if (type == "brookTrout") {

        if (element == "N") {

          message("You select McCutchan's et al (2003) Brook Trout tissue d13C: 8 values with 3.3 mean +- 0.29 se")
          return(brookTroutN)

        } else if (element == "C") {

          message("You select McCutchan's et al (2003) Brook Trout tissue d15N: 8 values with 3.8 mean +- 0.17 se")
          return(brookTroutC)

        } else if (element == "both") {

          message("You select McCutchan's et al (2003) Brook Trout tissue d15N: 8 values with 3.8 mean +- 0.17 se")
          message("and also McCutchan's et al (2003) Brook Trout tissue d13C: 8 values with 3.3 mean +- 0.29 se")
          return(list(deltaN = brookTroutN, deltaC = brookTroutC))
        }

      }


 }

}
