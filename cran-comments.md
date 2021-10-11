## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This release addresses a comment from Uwe Ligges upon the last submission that I should avoid skipping tests on CRAN.
* I've updated the tests to use vcr where possible, and to only skip a few tests if run on CRAN or offline.