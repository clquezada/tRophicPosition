#' Internal function that plot a mcmc.list object.
#'
#' Not intended to be used by the user.
#'
#' @param x null
#' @param trace null
#' @param density null
#' @param smooth null
#' @param bwf null
#' @param auto.layout null
#' @param ask null
#' @param ... null
#'
plotMCMC <- function (x, trace = TRUE, density = TRUE, smooth = TRUE, bwf,
                      auto.layout = TRUE, ask = graphics::par("ask"), ...)
  {
    ## RGA fixed to use default ask value.
    oldpar <- NULL
    on.exit(graphics::par(oldpar))
    if (auto.layout) {
      mfrow <- set.mfrow(Nchains = nchain(x), Nparms = nvar(x),
                         nplots = trace + density)
      oldpar <- graphics::par(mfrow = mfrow)
    }
    for (i in 1:nvar(x)) {
      if (trace)
        ## RGA fixed to propagate ... argument.
        traceplot(x[, i, drop = FALSE], smooth = smooth, ...)
      if (density) {
        if (missing(bwf))
          ## RGA fixed to propagate ... argument.
          densplot(x[, i, drop = FALSE], ...)
        else densplot(x[, i, drop = FALSE], bwf = bwf, ...)
      }
      if (i==1)
        oldpar <- c(oldpar, graphics::par(ask = ask))
    }
  }

#' Internal function that set.mfrow.
#'
#' Not intended to be used by the user.
#'
#' @param Nchains null
#' @param Nparms null
#' @param nplots null
#' @param sepplot null

set.mfrow <-
  function (Nchains = 1, Nparms = 1, nplots = 1, sepplot = FALSE)
  {
    ## Set up dimensions of graphics window:
    ## If only density plots OR trace plots are requested, dimensions are:
    ##	1 x 1	if Nparms = 1
    ##	1 X 2 	if Nparms = 2
    ##	2 X 2 	if Nparms = 3 or 4
    ##	3 X 2 	if Nparms = 5 or 6 or 10 - 12
    ##	3 X 3 	if Nparms = 7 - 9 or >= 13
    ## If both density plots AND trace plots are requested, dimensions are:
    ##	1 x 2	if Nparms = 1
    ##	2 X 2 	if Nparms = 2
    ##	3 X 2 	if Nparms = 3, 5, 6, 10, 11, or 12
    ##	4 x 2	if Nparms otherwise
    ## If separate plots are requested for each chain, dimensions are:
    ##	1 x 2	if Nparms = 1 & Nchains = 2
    ##	2 X 2 	if Nparms = 2 & Nchains = 2 OR Nparms = 1 & Nchains = 3 or 4
    ##	3 x 2	if Nparms = 3 or >= 5 & Nchains = 2
    ##		   OR Nchains = 5 or 6 or 10 - 12 (and any Nparms)
    ##	2 x 3	if Nparms = 2 or 4 & Nchains = 3
    ##	4 x 2   if Nparms = 4 & Nchains = 2
    ##		   OR Nchains = 4 & Nparms > 1
    ##	3 x 3	if Nparms = 3 or >= 5  & Nchains = 3
    ##		   OR Nchains = 7 - 9 or >= 13 (and any Nparms)
    mfrow <- if (sepplot && Nchains > 1 && nplots == 1) {
      ## Separate plots per chain
      ## Only one plot per variable
      if (Nchains == 2) {
        switch(min(Nparms, 5),
               c(1,2),
               c(2,2),
               c(3,2),
               c(4,2),
               c(3,2))
      }
      else if (Nchains == 3) {
        switch(min(Nparms, 5),
               c(2,2),
               c(2,3),
               c(3,3),
               c(2,3),
               c(3,3))
      }
      else if (Nchains == 4) {
        if (Nparms == 1)
          c(2,2)
        else
          c(4,2)
      }
      else if (any(Nchains == c(5,6,10,11,12)))
        c(3,2)
      else if (any(Nchains == c(7,8,9)) || Nchains >=13)
        c(3,3)

    }
    else {
      if (nplots==1) {
        ## One plot per variable
        mfrow <- switch(min(Nparms,13),
                        c(1,1),
                        c(1,2),
                        c(2,2),
                        c(2,2),
                        c(3,2),
                        c(3,2),
                        c(3,3),
                        c(3,3),
                        c(3,3),
                        c(3,2),
                        c(3,2),
                        c(3,2),
                        c(3,3))
      }
      else {
        ## Two plot per variable
        ##
        mfrow <- switch(min(Nparms, 13),
                        c(1,2),
                        c(2,2),
                        c(3,2),
                        c(4,2),
                        c(3,2),
                        c(3,2),
                        c(4,2),
                        c(4,2),
                        c(4,2),
                        c(3,2),
                        c(3,2),
                        c(3,2),
                        c(4,2))
      }
    }
    return(mfrow)
  }
