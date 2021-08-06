context("gnrs_get_countries")

safe_nrow <- function(x){
  if(is.null(x)) {
    return(0)}else{
      return(nrow(x))
    }
  
}


test_that("example works", {
  
  skip_if_offline()
  
  countries <- GNRS_get_countries()

  expect_equal(object = class(countries), expected = "data.frame")
  
  expect_gt(object = safe_nrow(countries),expected = 100)
  
})

