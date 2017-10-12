## Test environments
* local x86_64-apple-darwin15.6.0 (64-bit) install, R 3.4.2
* local x86_64-w64-mingw32/x64 (64-bit) install, R 3.4.2
* local x86_64-pc-linux-gnu (64-bit) install, R 3.4.0
* R CMD check on CRAN’s servers with devtools::build_win() (x86_64-w64-mingw32, under development 2017-09-12 r73242)

## R CMD check results
There were no ERRORs or WARNINGs in local and remote installs.

## Downstream dependencies
No ERRORs or WARNINGs found with devtools::revdep_check()
