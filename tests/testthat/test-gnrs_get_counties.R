context("gnrs_get_counties")

safe_nrow <- function(x){
  if(is.null(x)) {
    return(0)}else{
      return(nrow(x))
      }
  
}

test_that("example works", {
  
  # skip_if_offline()
  # skip_on_cran()
  # 
    
  vcr::use_cassette("states_for_counties",
                    { states <- GNRS_get_states(url = url) })
  
  vcr::use_cassette("us_counties",
                    {   us_counties <- GNRS_get_counties(state_province_id =
                                                           states$state_province_id[
                                                             which(states$country_iso == "US")],
                                                         url = url) })
  
  expect_equal(object = class(us_counties), expected = "data.frame")
  
  expect_gt(object = safe_nrow(us_counties),expected = 100)
  
})


test_that("default input returns data.frame", {
  
  # skip_if_offline()
  # skip_on_cran()
  
  vcr::use_cassette("all_counties",
                    {   counties <- GNRS_get_counties(url = url) })
  
  expect_equal(object = class(counties), expected = "data.frame")

  expect_gt(object = safe_nrow(counties), expected = 1000)
  
})

test_that("bad input returns error or NULL", {
  
  vcr::use_cassette("bad_state_id",
                    {   bad_state_id <- GNRS_get_counties(1, url = url) })
  
  expect_null(object = bad_state_id)
  
  expect_error(object = GNRS_get_counties("Optimus Prime", url = url))

})

