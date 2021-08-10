context("gnrs_data_dictionary")


test_that("example works", {
  
  skip_if_offline()
  
  GNRS_dictionary <- GNRS_data_dictionary()
  
  expect_equal(object = class(GNRS_data_dictionary()), expected = "data.frame")
  
  expect_gt(object = nrow(GNRS_dictionary),expected = 1)
  
})
