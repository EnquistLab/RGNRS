## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* This version update is in response to https://cran.r-project.org/web/checks/check_results_GNRS.html
* The issues highlighted in the CRAN check stem from tests failing when the API is briefly down
* To fix this problem, I've added skip_on_cran() to the tests, per the suggestion in the testthat documentation.
* All tests are passed locally.
* Apologies for sending another update so soon and for failing to comply with the policies.