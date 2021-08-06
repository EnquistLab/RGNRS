context("gnrs_get_counties")

safe_nrow <- function(x){
  if(is.null(x)) {
    return(0)}else{
      return(nrow(x))
      }
  
}

test_that("example works", {
  
  skip_if_offline()
  
  states <- GNRS_get_states()
  us_counties <- GNRS_get_counties(state_province_id =
                                     states$state_province_id[
                                       which(states$country_iso == "US")])
  
  
  expect_equal(object = class(us_counties), expected = "data.frame")
  
  expect_gt(object = safe_nrow(us_counties),expected = 100)
  
})


test_that("default input returns data.frame", {
  
  skip_if_offline()
  
  counties <- GNRS_get_counties()

  expect_equal(object = class(counties), expected = "data.frame")

  expect_gt(object = safe_nrow(counties),expected = 1000)
  
})

test_that("bad input returns error or NULL", {
  
  skip_if_offline()
  
  expect_null(object = GNRS_get_counties(1))
  
  expect_error(object = GNRS_get_counties("Optimus Prime"))  
  
})

