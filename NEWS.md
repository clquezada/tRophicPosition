tRophicPosition v 0.8.0 (Release date: 2022-12-11)
==============

* Updating CRAN version (archived recently due to email error). Small fixes.

tRophicPosition v 0.7.7 (Release date: 2019-04-05)
==============

* Updating CRAN version (archived recently). Small fix regarding examples.

tRophicPosition v 0.7.6 (Release date: 2018-06-27)
==============

* Fixed [issue 69](https://github.com/clquezada/tRophicPosition/issues/69) (parametricTP() when using only one baseline). This small fix changes current version of tRophicPosition to 0.7.6 only in GitHub. CRAN version will be updated soon.

tRophicPosition v0.7.5 (Release date: 2018-01-29)
==============

* Added the function extractPredictiveData() to perform a posterior predictive model-checking procedure.
* Implemented code coverage and unit test.
* Improved the code (to make it clearer, considering width and the assignment operator).
* Changed some arguments (consumer instead of species, group instead of community and others) in loadIsotopeData(), extractIsotopeData() and other functions (while maintaining old arguments as compatibility)
* Added a a procedure for checking errors on priors arguments in jagsBayesianModel and related functions
* Removed MCMCvis from the Short guide to tRophicPosition vignette, as that package is no longer in CRAN.

tRophicPosition v0.7.3 (Release date: 2017-10-12)
==============

* Minor details to fulfil CRAN checks

tRophicPosition v0.7.2 (Release date: 2017-10-12)
==============

* Fixed loadIsotopeData() when loading species without a community to iterate from.
* Added stable isotope data examples (Finnish Lakes and Roach)
* Improved credibilityIntervals(). Now it accepts legend position (for TP and alpha plots), a grouping variable (to plot groups with different colours), manual colours (scale_colour_manual) when using group_by, and labels for the x axis.

tRophicPosition v0.7.0 (Release date: 2017-06-11)
==============

First release version submitted to CRAN.

* List of capabilities
