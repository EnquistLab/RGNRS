context("gnrs_get_states")


test_that("example works", {
  
  states <- GNRS_get_states()
  
  expect_equal(object = class(states), expected = "data.frame")
  
  expect_gt(object = nrow(states),expected = 100)
  
})


test_that("inputing specific countries works", {
  
  countries <- GNRS_get_countries()
  states <- GNRS_get_states(country_id = countries$country_id[1:10])
  
  expect_equal(object = class(states), expected = "data.frame")
  
  expect_gt(object = nrow(states),expected = 10)
  
})