[![](https://www.r-pkg.org/badges/version-ago/tRophicPosition?color=blue)](https://cran.r-project.org/package=tRophicPosition) [![](http://cranlogs.r-pkg.org/badges/tRophicPosition)](https://cran.r-project.org/package=tRophicPosition) [![](http://cranlogs.r-pkg.org/badges/grand-total/tRophicPosition)](https://cran.r-project.org/package=tRophicPosition) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1161826.svg)](https://doi.org/10.5281/zenodo.1161826) [![](https://travis-ci.org/clquezada/tRophicPosition.svg?branch=master)](https://travis-ci.org/clquezada/tRophicPosition) [![Coverage Status](https://img.shields.io/codecov/c/github/clquezada/tRophicPosition/master.svg)](https://codecov.io/github/clquezada/tRophicPosition?branch=master)

# tRophicPosition
`tRophicPosition` is a colaborative project of [Chris Harrod](http://harrodlab.net), [Andrew L. Jackson](https://github.com/AndrewLJackson), [Claudio Quezada-Romegialli](https://github.com/clquezada) and others. It consists of an R package incorporating a Bayesian model for the calculation of consumer trophic position using stable isotopes with one or two baselines. It uses the powerful approach of Markov Chain Monte Carlo simulations provided by [JAGS](http://mcmc-jags.sourceforge.net) and the statistical language [R](https://www.r-project.org).

As of 29th of January 2018, we are releasing the version 0.7.5 of the package, the fourth release version. If you find any error, please send it to trophicposition-support@googlegroups.com and/or raise an issue in the GitHub page. Also you can also send a direct email to clquezada at harrodlab.net.

If you are interested in joining the tRophicPosition support group, do it at https://groups.google.com/d/forum/trophicposition-support

## NEWS (27 June 2018)
* Fixed [issue 69](https://github.com/clquezada/tRophicPosition/issues/69) (parametricTP() when using only one baseline). This small fix changes current version of tRophicPosition to 0.7.6 only in GitHub. CRAN version will be updated soon.

## NEWS (29 January 2018)

* Added the function extractPredictiveData() to perform a posterior predictive model-checking procedure.
* Implemented code coverage and unit test.
* Improved the code (to make it clearer, considering width and the assignment operator).
* Changed some arguments (consumer instead of species, group instead of community and others) in loadIsotopeData(), extractIsotopeData() and other functions (while maintaining old arguments as compatibility)
* Added a a procedure for checking errors on priors arguments in jagsBayesianModel and related functions
* Removed MCMCvis from the Short guide to tRophicPosition vignette, as that package is no longer in CRAN.

## NEWS (12 October 2017)

* Added the function fromParallelTP() to extract the data from parallel calculations (see the examples on [GitHub](https://github.com/clquezada/tRophicPosition/wiki/tRophicPosition-examples)).
* Implemented the Bhattacharrya Coefficient (from the package [dispRity](https://github.com/TGuillerme/dispRity)) when comparing two or more posterior distributions. As [dispRity](https://github.com/TGuillerme/dispRity) is only available through GitHub, when users have not installed the package we use the internal function bhat.coeff based on [dispRity](https://github.com/TGuillerme/dispRity) package.
* Improved credibilityIntervals(). Now it can group samples by colour, accept manual colours and legend position.
* Added two example datasets: Inari and Kilpis (Finnish_Lakes), and Roach, and including some examples in the [GitHub](https://github.com/clquezada/tRophicPosition/wiki/tRophicPosition-examples) page.

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

## Online vignettes
Can be found in GitHub [here](https://github.com/clquezada/tRophicPosition/wiki/tRophicPosition-vignettes).

## Credits
`tRophicPosition` relies on some code from [coda](https://cran.r-project.org/package=coda) and [dispRity](https://github.com/TGuillerme/dispRity). At the moment [dispRity](https://github.com/TGuillerme/dispRity) is only available through GitHub, so it is not included in Suggests. `tRophicPosition` included the Bhattacharrya Coefficient (bhatt.coeff function from [dispRity](https://github.com/TGuillerme/dispRity)) and plotMCMC (from [coda](https://cran.r-project.org/package=coda)) for convenience.

## Citation
When citing `tRophicPosition` in publications, please cite both the paper describing the method and the package version you used to analyse your data:
  
Claudio Quezada-Romegialli, Andrew L Jackson, Brian Hayden, Kimmo K Kahilainen, Christelle Lopes and Chris Harrod (2018). `tRophicPosition` , an R package for the Bayesian estimation of trophic position from consumer stable isotope ratios. Methods in Ecology and Evolution, 9(6):1592-1599. doi:[10.1111/2041-210X.13009](http://doi.org/10.1111/2041-210X.13009)

Claudio Quezada-Romegialli, Andrew L Jackson, Brian Hayden, Kimmo K Kahilainen, Christelle Lopes and Chris Harrod (2018).
tRophicPosition: Bayesian Trophic Position Calculation with Stable Isotopes. R package version 0.7.5. [https://cran.r-project.org/package=tRophicPosition](https://cran.r-project.org/package=tRophicPosition) doi:[10.5281/zenodo.1161826](https://doi.org/10.5281/zenodo.1161826)
  
For previous releases please use the corresponding version and doi when citing:

* v0.7.5 (29 January 2018) [10.5281/zenodo.1161826](https://doi.org/10.5281/zenodo.1161826)
  
* v0.7.3 (12 October 2017) [10.5281/zenodo.1009571](https://doi.org/10.5281/zenodo.1009571)

* v0.7.0 (12 June 2017) [10.5281/zenodo.806048](https://doi.org/10.5281/zenodo.806048)

## Future releases and how to get support

You are encouraged to use `tRophicPosition` with your own data, test the package and see if there are any issues or problems. You can send your questions or commentaries to the google group [tRophicPosition-support](https://groups.google.com/d/forum/trophicposition-support) or directly to the email trophicposition-support at googlegroups.com. You can send your questions to http://stackexchange.com/ http://stackoverflow.com/ or even [Facebook (stable isotope ecology group)](https://www.facebook.com/groups/stableisotopes/).

We are constantly working on future releases of `tRophicPosition`, so feedback is very much appreciated.

## To DO

Things [to do](https://github.com/clquezada/tRophicPosition/wiki/Things-to-do-in-tRophicPosition) in tRophicPosition
