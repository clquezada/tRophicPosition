## Test environments [2022-12-11]
* local x86_64-apple-darwin17.0 (64-bit) install, R 4.0.1
* local x86_64-w64-mingw32/x64 (64-bit) install, R 4.2.2
* CRAN’s servers with devtools::check_win_release()
* CRAN’s servers with devtools::check_win_devel()
* CRAN’s servers with rhub::check()



## R CMD check results  [2022-12-11]
There were no ERRORs or WARNINGs in local and remote installs.

* R CMD check on CRAN’s servers with devtools::check_win_release() 
[using R version 4.2.2 (2022-10-31 ucrt); platform: x86_64-w64-mingw32 (64-bit)]
* NOTES
checking CRAN incoming feasibility
Maintainer: 'Claudio Quezada-Romegialli <clquezada@ug.uchile.cl>'
New submission
Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  MCMC (15:29)
  Trophic (3:17)
  trophic (13:28)
  
Found the following (possibly) invalid URLs:
  URL: http://mcmc-jags.sourceforge.net (moved to https://mcmc-jags.sourceforge.io/)
    From: README.md
    Status: 200
    Message: OK
  URL: https://doi.org/10.1111/2041-210X.13009
    From: README.md
    Status: 503
    Message: Service Unavailable

* R CMD check on CRAN’s servers with devtools::check_win_devel() 
[R Under development (unstable) (2022-12-10 r83428 ucrt); using platform: x86_64-w64-mingw32 (64-bit)]


* R CMD check on CRAN’s servers with rhub::check() (Debian Linux, R-release, GCC (debian-gcc-release)))
* checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘SIBER’


## Downstream dependencies  [2022-12-11]
No ERRORs or WARNINGs found with usethis::use_revdep()
