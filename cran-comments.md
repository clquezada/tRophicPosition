## Test environments
* local x86_64-apple-darwin15.6.0 (64-bit) install, R 3.5.3
* local x86_64-w64-mingw32/x64 (64-bit) install, R 3.5.3
* R CMD check on CRAN’s servers with devtools::check_win_release(pkg = ".") (x86_64-w64-mingw32, R version 3.5.3 [2019-03-11])

## R CMD check results
There were no ERRORs or WARNINGs in local and remote installs.
NOTE: package was archived on CRAN as check problems were not corrected in time.

## Downstream dependencies
No ERRORs or WARNINGs found with devtools::revdep_check()
