context("gnrs_data_dictionary")

test_that("example works", {
  
  skip_if_offline()
  skip_on_cran()
  
  GNRS_dictionary <- GNRS_data_dictionary(url = url)
  
  expect_equal(object = class(GNRS_dictionary), expected = "data.frame")
  
  
  #test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(GNRS_dictionary)== "data.frame")
  expect_gt(object = nrow(GNRS_dictionary),expected = 1)
  
})
