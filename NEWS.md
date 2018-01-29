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
