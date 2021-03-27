context("gnrs_get_countries")


test_that("example works", {
  
  countries <- GNRS_get_countries()

  expect_equal(object = class(countries), expected = "data.frame")
  
  expect_gt(object = nrow(countries),expected = 100)
  
})