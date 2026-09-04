context("alternate forms of submitted names")

# The expected values are what the web service returned in the
# state_province_verbatim_alt and county_parish_verbatim_alt columns for the
# same submissions.

test_that("state names lose their administrative-class words", {
  x <- c(
    "Estado de Sonora", "Provincia de Lima", "Distrito Capital de Bogotá",
    "Moscow Oblast", "Regional District of Nanaimo", "Central Province ",
    "Sonora, Estado de", "Arizona", "", NA
  )
  expect_equal(
    gnrs_state_verbatim_alt(x),
    c("Sonora", "Lima", "Bogotá", "Moscow", "Nanaimo", "Central Province ", "", "", "", "")
  )
})

test_that("county names lose theirs, and the restored list is honoured", {
  x <- c(
    "Santa Fe de Antioquia", "Dobrovskiy rayon", "Municipio de Hermosillo",
    "Hermosillo Municipality", "Pima County", "Regional District of Nanaimo",
    "Comuna 5 Norte", "San Jeronimo Department", "Santa Barbara Do Sul",
    "Moscow Oblast", "", NA
  )
  expect_equal(
    gnrs_county_verbatim_alt(x),
    c(
      "Antioquia", "Dobrovskiy", "Hermosillo", "Hermosillo", "Pima", "Nanaimo",
      "Comuna 5 Norte", "San Jeronimo", "Santa Barbara Do Sul", "", "", ""
    )
  )
})

test_that("the first rule that fires wins, and later groups only fill blanks", {
  # "Distrito Capital" is protected from the bare "Distrito" prefix rule
  expect_equal(gnrs_state_verbatim_alt("Distrito Capital"), "")
  # A prefix rule fires before a suffix rule could
  expect_equal(gnrs_state_verbatim_alt("Provincia de Lima Province"), "Lima Province")
  # Case-insensitive, first occurrence only
  expect_equal(gnrs_county_verbatim_alt("MUNICIPIO DE Hermosillo"), "Hermosillo")
})

test_that("LIKE patterns are translated faithfully", {
  expect_true(gnrs_ilike("estado de sonora", "Estado de%"))
  expect_false(gnrs_like("estado de sonora", "Estado de%"))
  expect_true(gnrs_like("Muhafazat ash ", "Muhafazat ash "))
  expect_false(gnrs_like("Muhafazat ash x", "Muhafazat ash "))
  expect_true(gnrs_ilike("a.b", "a.b"))
  expect_false(gnrs_ilike("axb", "a.b"))
})
