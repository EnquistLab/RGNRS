context("gnrs_metadata")


test_that("example works", {
  
  #skip_if_offline()
  #skip_on_cran()
  
  
  vcr::use_cassette(name = "all_metadata",
                    {       all_metadata <- GNRS_metadata(url = url) })
  
  expect_equal(object = class(all_metadata), expected = "list")
  
  expect_equal(object = length(all_metadata), expected = 4)
  
})
