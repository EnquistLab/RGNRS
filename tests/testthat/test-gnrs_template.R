context("gnrs_template")


test_that("example works", {
  
  # skip_if_offline()
  # skip_on_cran()
  
  
  template <- GNRS_template(nrow = 2)
  template$country <- c("United Stapes","Mexico")
  template$state_province <- c("Arizona","Sinalo")
  
  
  vcr::use_cassette("template",
                    { results <- GNRS(political_division_dataframe = template) })

  expect_equal(object = class(results), expected = "data.frame")
  expect_equal(object = nrow(results), expected = 2)



})
