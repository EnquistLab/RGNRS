context("gnrs_get_countries")

safe_nrow <- function(x){
  if(is.null(x)) {
    return(0)}else{
      return(nrow(x))
    }
  
}


test_that("example works", {
  
  # skip_if_offline()
  # skip_on_cran()
  
  
  vcr::use_cassette("countries",
                    {   countries <- GNRS_get_countries(url = url) })
  
  expect_equal(object = class(countries), expected = "data.frame")
  
  expect_gt(object = safe_nrow(countries),expected = 100)
  
})

