context("gnrs_get_states")

safe_nrow <- function(x){
  if(is.null(x)) {
    return(0)}else{
      return(nrow(x))
    }
  
}


test_that("example works", {
  
  skip_if_offline()
  skip_on_cran()
  
  
  states <- GNRS_get_states(url = url)
  
  expect_equal(object = class(states), expected = "data.frame")
  
  expect_gt(object = safe_nrow(states),expected = 100)
  
})


test_that("inputing specific countries works", {
  
  skip_if_offline()
  skip_on_cran()
  
  
  countries <- GNRS_get_countries(url = url)
  
  states <- GNRS_get_states(country_id = countries$country_id[1:10])
  
  expect_equal(object = class(states), expected = "data.frame")
  
  expect_gt(object = safe_nrow(states),expected = 10)
  
})

