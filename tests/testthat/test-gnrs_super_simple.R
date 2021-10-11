context("gnrs_super_simple")


test_that("example works", {
  
  #skip_if_offline()
  #skip_on_cran()
  
  vcr::use_cassette("super_simple_usa",
                    {   results <- GNRS_super_simple(country = "United States of America", url = url) })
  
  expect_equal(object = class(results), expected = "data.frame")

  expect_equal(object = nrow(results),expected = 1)

  vcr::use_cassette("super_simple_pima",
                    {     results <- GNRS_super_simple(
                      country = "United States",
                      state_province = "Arizona",
                      county_parish = "Pima County",
                      url = url) })
  
  expect_equal(object = class(results), expected = "data.frame")

  expect_equal(object = nrow(results), expected = 1)

})


test_that("bad input returns no match", {
  
  # skip_if_offline()
  # skip_on_cran()
  
  vcr::use_cassette("ssuper_simple_nonsense",
                    {  nonsense_query <- 
                      GNRS_super_simple(country = "Autobot",
                                        state="Optimus",
                                        county_parish = "Prime",
                                        user_id = 2005,
                                        url = url)})
  
  expect_equal(object = nonsense_query$match_status,
               expected = "no match")

})

test_that("Good input returns a match", {
  
  # skip_if_offline()
  # skip_on_cran()
  
  vcr::use_cassette("super_simple_sensible",
                    {   sensible_query <- 
                      GNRS_super_simple(country = "USA",
                                        state="Michigan",
                                        county_parish = "Kent",
                                        user_id = 616,
                                        url = url)})
  
  expect_equal(object = sensible_query$match_status,
               expected = "full match",
               url = url)
  
})

