## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* This release addresses a comment I received upon the last submission that I should avoid skipping tests on CRAN.
* To avoid skipping tests, I've updated the tests to use vcr where possible, and to only skip a few tests if run on CRAN or offline.
* R-hub errors: When checking with Rhub, an error occurs (bioconductor does not yet build and  check packages for R version 4.2) which seems to be related to the testing environment and not the package . This looks to be a known issue (https://github.com/r-hub/rhub/issues/471) and others have noted that they have submitted to CRAN while it is present.