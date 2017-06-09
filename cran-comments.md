## Test environments
* local x86_64-apple-darwin13.4.0 install, R 3.3.2
* local x86_64-w64-mingw32/x64 (64-bit) [Windows >= 8 x64 (build 9200)] install, R 3.4.0 (2017-04-21)
* R CMD check on CRAN’s servers with devtools::build_win()

## R CMD check results
There were no ERRORs or WARNINGs in local installs.

R CMD check on CRAN's server gave 1 note (not an error though):

* Possibly mis-spelled words in DESCRIPTION:
    + MCMC (15:14)
    + Trophic (3:17)
    + rjags (15:30)
    + trophic (13:28)

## Downstream dependencies
No ERRORs or WARNINGs found with devtools::revdep_check()
