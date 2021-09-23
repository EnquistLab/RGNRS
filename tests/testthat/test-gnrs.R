context("gnrs")

safe_nrow <- function(x){
  if(is.null(x)) {
    return(0)}else{
      return(nrow(x))
    }
  
}


test_that("example works", {
  
  skip_if_offline()

  results <- GNRS(political_division_dataframe = gnrs_testfile, url = url)
  
  expect_equal(object = class(results), expected = "data.frame")
  
  expect_equal(object = safe_nrow(results), expected = nrow(gnrs_testfile))

  results <- GNRS_super_simple(
                country = "United States",
                state_province = "Arizona",
                county_parish = "Pima County",
                url = url)
  
  expect_equal(object = class(results), expected = "data.frame")

  expect_equal(object = safe_nrow(results),expected = 1)

})


test_that("bad input returns error", {
  
  skip_if_offline()
  
  expect_error(object = GNRS(political_division_dataframe = 1,
                             url = url))  
  

})