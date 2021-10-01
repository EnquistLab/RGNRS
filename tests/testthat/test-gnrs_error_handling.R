# test to make sure a server being down doesn't throw an error, but fails with a message


context("error-handling")


test_that("server being down fails gracefully", {
  
  skip_if_offline()
  
  expect_null(object = suppressMessages(GNRS(political_division_dataframe = gnrs_testfile,
                                             url = "http://www.google.com")))
  

  
  expect_message(object = GNRS(political_division_dataframe = gnrs_testfile,
                               url =  "http://www.google.com"))
  
  expect_null(object = suppressMessages(GNRS_get_countries(url =  "http://www.google.com")))
  expect_message(object = GNRS_get_countries(url =  "http://www.google.com"))
  

  expect_null(suppressMessages(GNRS_metadata(url= "http://www.google.com")))
  expect_message(object = GNRS_metadata(url= "http://www.google.com"))  


  
  expect_null(object = suppressMessages(GNRS(political_division_dataframe = gnrs_testfile,
                                             url = "http://www.brianisgreat.com")))
  
  
  
  expect_message(object = GNRS(political_division_dataframe = gnrs_testfile,
                               url =  "http://www.brianisgreat.com"))
  
  
})


test_that("bad input fails gracefully", {
  
  skip_if_offline()
  
  expect_null(object = suppressMessages(GNRS(political_division_dataframe = gnrs_testfile[,-2], url = url)))
  expect_message(object = GNRS(political_division_dataframe = gnrs_testfile[,-2], url = url))
  
  expect_null(suppressMessages(GNRS_get_counties(state_province_id = 1, url = url)))
  expect_message(GNRS_get_counties(state_province_id = 1, url = url))
  
  expect_null(suppressMessages(GNRS_get_states(country_id = 1, url = url)))
  expect_message(GNRS_get_states(country_id = 1, url = url))
  
  
})