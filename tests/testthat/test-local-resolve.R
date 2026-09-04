context("offline resolution")

# Against the synthetic reference in helper-local.R.  Each expectation is a
# behaviour the web service showed for the same kind of submission; the
# match-method labels are the service's.

dir <- gnrs_test_backbone()

test_that("the output has the web service's columns, in its order", {
  r <- gnrs_test_resolve(dir, "United States", "Arizona", "Pima County")
  expect_equal(names(r), c(
    "poldiv_full", "country_verbatim", "state_province_verbatim",
    "state_province_verbatim_alt", "county_parish_verbatim",
    "county_parish_verbatim_alt", "country", "state_province", "county_parish",
    "country_id", "state_province_id", "county_parish_id", "country_iso",
    "state_province_iso", "county_parish_iso", "geonameid", "gid_0", "gid_1",
    "gid_2", "match_method_country", "match_method_state_province",
    "match_method_county_parish", "match_score_country",
    "match_score_state_province", "match_score_county_parish", "threshold_fuzzy",
    "overall_score", "poldiv_submitted", "poldiv_matched", "match_status", "user_id"
  ))
  expect_equal(r$poldiv_full, "United States@Arizona@Pima County")
  expect_equal(r$country_id, "6252001")
  expect_equal(r$state_province_id, "5551752")
  expect_equal(r$county_parish_id, "5308878")
  expect_equal(r$county_parish, "Pima")
  expect_equal(r$county_parish_iso, "019")
  expect_equal(r$state_province_iso, "AZ")
  expect_equal(r$geonameid, "5308878")
  expect_equal(r$gid_2, "USA.3.11_1")
  expect_equal(unlist(r[c("match_method_country", "match_method_state_province", "match_method_county_parish")], use.names = FALSE),
    c("exact standard name", "exact name", "exact name"))
  expect_equal(r$overall_score, 1)
  expect_equal(r$threshold_fuzzy, 0.5)
  expect_equal(r$match_status, "full match")
  expect_equal(r$poldiv_matched, "county_parish")
})

test_that("exact steps run in the upstream order and label as upstream does", {
  r <- gnrs_test_resolve(dir,
    country = c("USA", "US", "united states", "México", "Mexico", "Reino Unido", "America"),
    state = c("AZ", "US.AZ", "arizona", "Sonora", "Estado de Sonora", "Escocia", "GA"),
    county = c("Pima", "019", "PIMA COUNTY", "Huépac", "Huepac", "Fife", "")
  )
  expect_equal(r$match_method_country, c(
    "iso_alpha3 code", "iso code", "exact standard name", "exact ascii name",
    "exact standard name", "exact alternate name", "exact alternate name"
  ))
  expect_equal(r$match_method_state_province, c(
    "iso code", "full iso code", "exact name", "exact ascii short name",
    "exact name", "exact alternate name", "iso code"
  ))
  expect_equal(r$match_method_county_parish, c(
    "exact ascii short name", "iso code", "exact name", "exact name",
    "exact ascii name", "exact name", ""
  ))
  expect_equal(r$state_province, c("Arizona", "Arizona", "Arizona", "Sonora", "Sonora", "Scotland", "Georgia"))
  expect_true(all(r$overall_score == 1))
})

test_that("alternate codes and the full/short distinction are honoured", {
  r <- gnrs_test_resolve(dir, c("GBR", "GB"), c("GB-ENG", "ENG"), c("DEV", "D4"))
  expect_equal(r$match_method_state_province, c("full alternate code", "iso code"))
  expect_equal(r$match_method_county_parish, c("alternate code", "iso code"))
  expect_equal(r$county_parish, c("Devon", "Devon"))
})

test_that("fuzzy matching scores as the service does and respects the threshold", {
  r <- gnrs_test_resolve(dir, c("Unted States", "Untied States", "Mexico"), c("Arizonna", "", "Sonra"), c("Pima Cnty", "", "Hermosilo"))
  expect_equal(r$match_method_country[1:2], c("fuzzy standard name", "fuzzy standard name"))
  expect_equal(r$match_score_country[1:2], c(0.69, 0.56))
  expect_equal(r$match_method_state_province[1], "fuzzy standard name")
  expect_equal(r$match_score_state_province[1], 0.7)
  # "Pima Cnty" contains the short name "Pima", which is unique in Arizona
  expect_equal(r$match_method_county_parish[1], "wildcard alt name")
  expect_equal(r$match_score_county_parish[1], 1)
  expect_equal(r$overall_score[1], 0.8)
  # Sonra is too far from Sonora; the county is then not attempted
  expect_equal(r$state_province[3], "")
  expect_equal(r$match_score_state_province[3], 0)
  expect_equal(r$match_score_county_parish[3], 0)
  expect_equal(r$overall_score[3], 0.33)
  expect_equal(r$match_status[3], "partial match")

  strict <- gnrs_test_resolve(dir, "Unted States", threshold = 0.7)
  expect_equal(strict$country, "")
  expect_equal(strict$match_status, "no match")
  expect_equal(strict$threshold_fuzzy, 0.7)
})

test_that("a state that is really a country becomes the country", {
  r <- gnrs_test_resolve(dir, c("United States", "United States", "Puerto Rico"), c("Puerto Rico", "Puerto Rico", ""), c("", "Mayaguez", "Mayaguez"))
  expect_equal(r$country, c("Puerto Rico", "Puerto Rico", "Puerto Rico"))
  expect_equal(r$match_method_country[1], "exact standard name, state-as-country")
  expect_equal(r$state_province[1], "")
  expect_equal(r$poldiv_matched[1], "state-as-country")
  expect_equal(r$match_status[1], "full match")
  # The country score is the similarity of the submitted state to the
  # resolved country; the state was submitted but not matched, so scores 0
  expect_equal(r$match_score_country[1], 1)
  expect_equal(r$match_score_state_province[1], 0)
  expect_equal(r$overall_score[1], 0.5)
  # With a county, that county is looked for among the country's states
  expect_equal(r$match_method_state_province[2], "exact standard name, county-as-state")
  expect_equal(r$state_province[2], "Mayaguez")
  expect_equal(r$poldiv_matched[2], "county-as-state")
  expect_equal(r$overall_score[2], 0.67)
  # A county named for a state, with no state given, is taken as the state
  expect_equal(r$match_method_state_province[3], "wildcard state-in-county-field")
  expect_equal(r$state_province[3], "Mayaguez")
  expect_true(is.na(r$match_score_state_province[3]))
  expect_equal(r$match_score_county_parish[3], 0)
  expect_true(is.na(r$overall_score[3]))
  expect_equal(r$match_status[3], "partial match")
})

test_that("a constituent country of the UK is a state, and its state a county", {
  r <- gnrs_test_resolve(dir, c("England", "Scotland", "Scotland", "United Kingdom"), c("Devon", "", "Fife", "England"), c("", "", "", "Devon"))
  expect_equal(r$country, rep("United Kingdom", 4))
  expect_equal(r$match_method_country[1:3], rep("inferred from country-as-state", 3))
  expect_equal(r$match_method_state_province[1:3], rep("exact standard name, country-as-state", 3))
  expect_equal(r$county_parish, c("Devon", "", "Fife", "Devon"))
  expect_equal(r$match_method_county_parish[1], "exact standard name, state-as-county")
  expect_equal(r$poldiv_matched, c("state-as-county", "country-as-state", "state-as-county", "county_parish"))
  expect_equal(r$match_status, c("full match", "full match", "full match", "full match"))
  expect_equal(r$overall_score, c(1, 1, 1, 1))
})

test_that("a broken hierarchy does not resolve, and blanks score as the service scores them", {
  r <- gnrs_test_resolve(dir, c("", "", "Zzzz", "United States"), c("Arizona", "", "Qqqq", ""), c("", "Pima", "Wwww", ""))
  expect_equal(r$match_status, c("no match", "no match", "no match", "full match"))
  expect_equal(r$poldiv_submitted, c("state_province", "county_parish", "county_parish", "country"))
  expect_equal(r$match_score_state_province, c(0, NA, 0, NA))
  expect_equal(r$match_score_county_parish, c(NA, 0, 0, NA))
  expect_true(is.na(r$overall_score[1]))
  expect_equal(r$overall_score[3], 0)
  expect_equal(r$overall_score[4], 1)
  # An entirely blank row has nothing submitted and no status
  blank <- gnrs_test_resolve(dir, "", "", "")
  expect_equal(blank$poldiv_submitted, "")
  expect_equal(blank$match_status, "")
})

test_that("alternate names can be switched off", {
  with_alt <- gnrs_test_resolve(dir, "Reino Unido")
  without <- gnrs_test_resolve(dir, "Reino Unido", alternate_names = FALSE)
  expect_equal(with_alt$match_method_country, "exact alternate name")
  expect_equal(without$country, "")
  # The reference's own names are still there
  expect_equal(gnrs_test_resolve(dir, "United Kingdom", alternate_names = FALSE)$country, "United Kingdom")
})

test_that("divisions added from GADM are matched on their own names", {
  r <- gnrs_test_resolve(dir, c("Testland", "TST", "Testland"), c("Testregion", "Testregion", "TS.TR"), c("Testshire", "TS.TR.TS", ""))
  expect_equal(r$match_method_country, c("exact standard name", "iso_alpha3 code", "exact standard name"))
  expect_equal(r$match_method_county_parish[1:2], c("exact name", "full hasc code"))
  # The full HASC code matches; a short one does not, as on the service
  expect_equal(r$match_method_state_province[3], "full hasc code")
  expect_equal(gnrs_test_resolve(dir, "Mexico", "SO")$state_province, "")
  # No ISO code to report, and the GeoNames identifier is still the lowest matched
  expect_equal(r$country_iso, c("", "", ""))
  expect_equal(r$geonameid, c("90000003", "90000003", "90000002"))
})

test_that("input handling matches GNRS()", {
  df <- data.frame(user_id = c(NA, NA), country = c("Mexico", NA), state_province = c(NA, "Arizona"), county_parish = c(NA, NA))
  r <- GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE)
  expect_equal(r$user_id, c("1", "2"))
  expect_equal(r$country_verbatim, c("Mexico", ""))
  expect_equal(r$poldiv_full, c("Mexico@@", "@Arizona@"))

  # Repeated rows come back once each, in input order, with their own ids
  df <- data.frame(user_id = c("b", "a", "c"), country = c("Mexico", "Mexico", "Mexico"), state_province = c("Sonora", "Sonora", ""), county_parish = "")
  r <- GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE)
  expect_equal(r$user_id, c("b", "a", "c"))
  expect_equal(r$state_province, c("Sonora", "Sonora", ""))

  expect_error(GNRS_local(1, dir = dir), "data.frame")
  expect_error(GNRS_local(data.frame(user_id = c(1, 1), country = "a", state_province = "", county_parish = ""), dir = dir), "unique")
  expect_error(GNRS_local(data.frame(country = "a"), dir = dir), "state_province")
  expect_error(GNRS_local(GNRS_template(), threshold = 2, dir = dir), "threshold")
  expect_error(GNRS_local(GNRS_template(), alternate_names = NA, dir = dir), "alternate_names")
})

test_that("a missing backbone is reported rather than erroring", {
  empty <- file.path(tempdir(), "gnrs-empty-cache")
  unlink(empty, recursive = TRUE)
  dir.create(empty, showWarnings = FALSE)
  expect_message(
    r <- GNRS_local(GNRS_template(), dir = empty, build_missing = FALSE, quiet = TRUE),
    "GNRS_local_build"
  )
  expect_null(r)
})

test_that("status and citations describe what was built", {
  s <- suppressMessages(GNRS_local_status(dir))
  expect_equal(s$source, c("gnrs", "geonames"))
  expect_true(all(s$built))
  expect_equal(s$version[1], "database test (2024-01-01), code test")
  cit <- GNRS_local_citations(dir, quiet = TRUE)
  expect_equal(cit$what, c("method", "software", "source", "source"))
  expect_true(all(grepl("2024-01-01", cit$citation[3:4])))
  bib <- tempfile(fileext = ".bib")
  GNRS_local_citations(dir, bibtex_file = bib, quiet = TRUE)
  expect_true(any(grepl("^@article", readLines(bib))))

  tmp <- file.path(tempdir(), "gnrs-remove-me")
  dir.create(tmp, showWarnings = FALSE)
  file.create(file.path(tmp, "x"))
  expect_message(expect_true(GNRS_local_remove(tmp, ask = FALSE)), "Removed")
  expect_false(dir.exists(tmp))
  expect_message(expect_false(GNRS_local_remove(tmp, ask = FALSE)), "Nothing to remove")
})
