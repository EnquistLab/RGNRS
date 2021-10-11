context("gnrs_version")


test_that("example works", {
  
  # skip_if_offline()
  # skip_on_cran()
  
  vcr::use_cassette("version_metadata",
                    {   GNRS_version_metadata <- GNRS_version(url = url) })
  
  expect_equal(object = class(GNRS_version_metadata), expected = "data.frame")
  
  expect_equal(object = nrow(GNRS_version_metadata), expected = 1)

})
