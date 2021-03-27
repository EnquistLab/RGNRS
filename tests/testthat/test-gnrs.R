context("gnrs")


test_that("example works", {
  
  gnrs_testfile <- 
  read.csv(system.file("extdata",
                       "gnrs_testfile.csv",
                       package = "GNRS",
                       mustWork = TRUE),
           stringsAsFactors = FALSE)
  
  results <- GNRS(political_division_dataframe = gnrs_testfile)
  

  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results), expected = nrow(gnrs_testfile))


results <- GNRS_super_simple(
              country = "United States",
              state_province = "Arizona",
              county_parish = "Pima County")
  
expect_equal(object = class(results), expected = "data.frame")
expect_equal(object = nrow(results),expected = 1)

  
  
})


test_that("bad input returns error", {

  expect_error(object = GNRS(political_division_dataframe = 1))  
  

})