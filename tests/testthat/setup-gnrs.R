
# api urls

url <- "https://gnrsapi.xyz/gnrs_api.php" # public stable version
#url <- "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # public development production
#url <- "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" #bleeding edge development

#Bad URLs for testing

#url <- "www.google.com"
#url <- "www.hisstank.com"

library("vcr") # *Required* as vcr is set up on loading

invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()
