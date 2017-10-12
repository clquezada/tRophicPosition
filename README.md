[![](https://www.r-pkg.org/badges/version-ago/tRophicPosition?color=blue)](http://cran.rstudio.com/web/packages/tRophicPosition/index.html) [![](http://cranlogs.r-pkg.org/badges/tRophicPosition)](http://cran.rstudio.com/web/packages/tRophicPosition/index.html) [![DOI](https://zenodo.org/badge/49386535.svg)](https://zenodo.org/badge/latestdoi/49386535)

# tRophicPosition
`tRophicPosition`, is an R package incorporating a Bayesian model for the calculation of consumer trophic position using stable isotopes with one or two baselines. It uses the powerful approach of Markov Chain Monte Carlo simulations provided by [JAGS](http://mcmc-jags.sourceforge.net) and the statistical language [R](https://www.r-project.org).

As of 12th of October 2017, we are releasing the version 0.7.2 of the package, the second release version. If you find any error, please send it to trophicposition-support@googlegroups.com and/or raise an issue in the GitHub page. Also you can also send a direct email to clquezada at harrodlab.net.

If you are interested in joining the tRophicPosition support group, do it at https://groups.google.com/d/forum/trophicposition-support

## NEWS

* Added the function fromParallelTP() to extract the data from parallel calculations (see the examples).
* Implemented the Bhattacharrya Coefficient (from the package [dispRity](https://github.com/TGuillerme/dispRity)) when comparing two or more posterior distributions. As [dispRity](https://github.com/TGuillerme/dispRity) is only available through GitHub, when users have not installed the package we use the internal function bhat.coeff based on [dispRity](https://github.com/TGuillerme/dispRity) package.
* Improved credibilityIntervals(). Now it can group samples by colour, accept manual colours and legend position.
* Added two example datasets: Inari and Kilpis (Finnish_Lakes), and Roach, and including some examples in the GitHub page.

## Installation

The stable version of `tRophicPosition` is hosted on [CRAN](https://cran.r-project.org/package=tRophicPosition), and is installed like any other package:
```{r}
install.packages("tRophicPosition")
```

Remember to install [JAGS](http://mcmc-jags.sourceforge.net) for your platform as well.

The development version of `tRophicPosition` must be installed from GitHub. For this, install the package devtools, and then use the function `install_github()`:

```{r}
install.packages("devtools")
devtools::install_github("clquezada/tRophicPosition", build_vignettes = TRUE)
```

And then see the vignettes we have prepared to describe how to use the package:

```{r}
browseVignettes("tRophicPosition")
```

## Future releases and how to get support

You are encouraged to use `tRophicPosition` with your own data, test the package and see if there are any issues or problems. You can send your questions or commentaries to the google group [tRophicPosition-support](https://groups.google.com/d/forum/trophicposition-support) or directly to the email trophicposition-support at googlegroups.com. You can send your questions to http://stackexchange.com/ http://stackoverflow.com/ or even [Facebook (stable isotope ecology group)](https://www.facebook.com/groups/stableisotopes/).

We are constantly working on future releases of `tRophicPosition`, so feedback is very much appreciated.
