context("gnrs_version")


test_that("example works", {
  
  GNRS_version_metadata <- GNRS_version()
  
  expect_equal(object = class(GNRS_version_metadata), expected = "data.frame")
  expect_equal(object = nrow(GNRS_version_metadata), expected = 1)



})