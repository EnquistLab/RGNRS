context("offline resolution against the web service")

# These compare GNRS_local() with answers recorded from the web service.  They
# need the real reference data, which is a download, so they skip unless it
# has already been built; the synthetic tests in test-local-resolve.R cover
# the cascade itself without it.

skip_without_reference <- function() {
  skip_if_not(
    gnrs_is_built("gnrs") && gnrs_is_built("geonames"),
    "no local reference data; run GNRS_local_build()"
  )
  # With the current GADM laid over the service's tables, divisions the
  # service lacks can match, so the answers are not expected to be the same
  skip_if(gnrs_is_built("gadm"), "the GADM layer is built; answers may differ from the service's")
}

test_that("the package test file resolves to the service's political divisions", {
  skip_without_reference()

  vcr::use_cassette("local_validation_testfile", {
    from_service <- GNRS(political_division_dataframe = gnrs_testfile, url = url)
  })
  skip_if(is.null(from_service), "no recorded service answer")

  local <- GNRS_local(gnrs_testfile, quiet = TRUE)
  from_service <- from_service[match(local$user_id, from_service$user_id), ]

  # Every identifier and resolved name agrees.  Match methods can differ
  # where the local GeoNames names are newer than the service's, so they are
  # not compared here.
  for (column in c(
    "country_id", "state_province_id", "county_parish_id",
    "country", "state_province", "county_parish",
    "poldiv_matched", "match_status", "geonameid", "gid_2"
  )) {
    expect_equal(local[[column]], from_service[[column]], info = column)
  }
  expect_equal(
    local$overall_score,
    suppressWarnings(as.numeric(from_service$overall_score))
  )
})

test_that("the cross-level routes give the service's answers", {
  skip_without_reference()

  submitted <- data.frame(
    user_id = 1:10,
    country = c(
      "United States", "United States", "England", "Scotland", "Wales",
      "China", "Peru", "USA", "GBR", "Unted States"
    ),
    state_province = c(
      "Puerto Rico", "Puerto Rico", "Devon", "", "",
      "Taiwan", "", "AZ", "GB-ENG", "Arizonna"
    ),
    county_parish = c(
      "", "Mayaguez", "", "", "",
      "Taipei", "Lima", "", "DEV", "Pima Cnty"
    ),
    stringsAsFactors = FALSE
  )
  vcr::use_cassette("local_validation_routes", {
    from_service <- GNRS(political_division_dataframe = submitted, url = url)
  })
  skip_if(is.null(from_service), "no recorded service answer")

  local <- GNRS_local(submitted, quiet = TRUE)
  from_service <- from_service[match(local$user_id, from_service$user_id), ]

  for (column in c(
    "country_id", "state_province_id", "county_parish_id",
    "match_method_country", "match_method_state_province",
    "match_method_county_parish", "poldiv_matched", "match_status"
  )) {
    expect_equal(local[[column]], from_service[[column]], info = column)
  }
  for (column in c(
    "match_score_country", "match_score_state_province",
    "match_score_county_parish", "overall_score"
  )) {
    expect_equal(
      local[[column]], suppressWarnings(as.numeric(from_service[[column]])),
      info = column
    )
  }
})
