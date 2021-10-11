# test to make sure a server being down doesn't throw an error, but fails with a message


context("error-handling")


test_that("server being down fails gracefully", {
  
  #skip_if_offline()
  #skip_on_cran()
  
  #Check for GNRS
  vcr::use_cassette("gnrs_google",
                    { gnrs_google <- suppressMessages(GNRS(political_division_dataframe = gnrs_testfile[1,],
                                          url = "http://www.google.com")) })

  expect_null(object = gnrs_google)
  

  #Check for GNRS_get_countries
  
  vcr::use_cassette("gnrs_countries_google",
                    { gnrs_countries_google <- suppressMessages(GNRS_get_countries(url =  "http://www.google.com")) })


  expect_null(object = gnrs_countries_google)
  
  #Check for GNRS_metadata
  
  vcr::use_cassette("gnrs_metadata_google",
                    { gnrs_metadata_google <- suppressMessages(GNRS_metadata(url= "http://www.google.com")) })

  expect_null(gnrs_metadata_google)
  
  
  #The checks below require internet access but don't rely on vcr-able output, so are skipped on cran or when offline
  
  
  skip_if_offline()
  skip_on_cran()
  
  expect_message(object = GNRS(political_division_dataframe = gnrs_testfile,
                               url =  "http://www.google.com"))
  
  expect_message(object = GNRS(political_division_dataframe = gnrs_testfile,
                               url =  "http://www.brianisgreat.com"))
  
  expect_message(object = GNRS_get_countries(url =  "http://www.google.com"))
  
  expect_message(object = GNRS_metadata(url= "http://www.google.com"))
  
  expect_null(object = suppressMessages(GNRS(political_division_dataframe = gnrs_testfile,
                                             url = "http://www.brianisgreat.com")))
  
  
})


test_that("bad input fails gracefully", {
  
  vcr::use_cassette("missing_column",
                    { missing_column <- suppressMessages(GNRS(political_division_dataframe = gnrs_testfile[1,-2], url = url)) })

  expect_null(object = missing_column)
  
  vcr::use_cassette("error_handling_bad_state_id",
                    { bad_state_id <- suppressMessages(GNRS_get_counties(state_province_id = 1, url = url)) })

  expect_null(bad_state_id)
  
  vcr::use_cassette("bad_country_id",
                    { bad_country_id <- suppressMessages(GNRS_get_states(country_id = 1, url = url)) })

  expect_null(bad_country_id)

  
  skip_if_offline()
  skip_on_cran()

  
  expect_message(object = GNRS(political_division_dataframe = gnrs_testfile[,-2], url = url))
  
  expect_message(GNRS_get_counties(state_province_id = 1, url = url))
  
  
  expect_message(GNRS_get_states(country_id = 1, url = url))
  
  
})

