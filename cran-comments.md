## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Changes for CRAN compliance

The previous version(s) of this package were not in CRAN compliance as they produced errors in testing when the internet was not available, or when the API was down or not working properly.

We have made substantial updates to both the package and the API to prevent these issues, including:

* Tests are now skipped if the internet is unavailable
* Functions will give an informative message and (invisibly) return NULL if the internet is not available
* The API has been updated to only return a HTTP status code of 200 when it will be returning properly formatted json
* The API status code is now checked before attempting to parse the JSON text (which was one source of problems), and parsing is only done when status == 200
* API queries are now wrapped in a trycatch(), and will give an informative message and (invisibly) return NULL if there is a problem with the API response
* Attempts to parse JSON text are now wrapped in trycatch(), and give an informative message and (invisibly) return NULL if the content isn't properly formatted
* We've revised our functions to take url as a parameter, allowing us to simulate a downed server or bad responses by changing the URL to websites that don't exist or won't return properly formatted responses.
* We've added automated tests to ensure that an inappropriate server response or server outage will fail gracefully by showing an error message and (invisibly) returning NULL. 
* With all these changes in place, we receive no errors or warnings when checking with check(), check_win_devel(), or check_rhub().