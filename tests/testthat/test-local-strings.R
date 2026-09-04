context("local string primitives")

# These reproduce the PostgreSQL functions the upstream SQL relies on:
# unaccent(), LOWER() and pg_trgm's similarity().  Expected values are worked
# by hand from the pg_trgm definition and checked against scores the web
# service returned for the same pairs.

test_that("gnrs_unaccent strips diacritics as unaccent() does", {
  expect_equal(
    gnrs_unaccent(c("Huépac", "Zitácuaro", "Québec", "Skåne", "Malmö")),
    c("Huepac", "Zitacuaro", "Quebec", "Skane", "Malmo")
  )
  # Ligatures and special letters expand rather than vanish
  expect_equal(gnrs_unaccent(c("Straße", "Ægir", "Ørsted", "Łódź")), c("Strasse", "AEgir", "Orsted", "Lodz"))
  # Vietnamese stacked diacritics
  expect_equal(gnrs_unaccent("Đà Nẵng"), "Da Nang")
  # Non-Latin scripts are left alone
  expect_equal(gnrs_unaccent("Россия"), "Россия")
  # Plain text and blanks pass through
  expect_equal(gnrs_unaccent(c("Arizona", "", NA)), c("Arizona", "", NA))
})

test_that("gnrs_trigrams forms pg_trgm trigrams as a set", {
  # Two spaces before each word, one after; the two words of "County Cork"
  # share their first two trigrams, which count once
  expect_equal(
    gnrs_trigrams("County Cork")[[1]],
    c("  c", " co", "cou", "oun", "unt", "nty", "ty ", "cor", "ork", "rk ")
  )
  # An apostrophe separates words, as any non-alphanumeric does
  expect_equal(gnrs_trigrams("d'Ivoire")[[1]][1:4], c("  d", " d ", "  i", " iv"))
  # Case is folded, and blanks have no trigrams at all
  expect_equal(gnrs_trigrams("PIMA")[[1]], gnrs_trigrams("pima")[[1]])
  expect_equal(gnrs_trigrams(c("", "  ", NA, "-")), list(character(0), character(0), character(0), character(0)))
})

test_that("gnrs_similarity reproduces similarity() scores the service returns", {
  # Values the web service returned as match scores for these pairs
  expect_equal(round(gnrs_similarity_pair("Untied States", "United States"), 4), 0.5556)
  expect_equal(gnrs_similarity_pair("County Cork", "Cork"), 0.5)
  expect_equal(round(gnrs_similarity_pair("PR", "Puerto Rico"), 4), 0.0714)
  expect_equal(round(gnrs_similarity_pair("Unted States", "United States"), 4), 0.6875)
  expect_equal(gnrs_similarity_pair("Arizonna", "Arizona"), 0.7)
  expect_equal(gnrs_similarity_pair("Zanzibar", "Zanzibar North"), 0.6)
  expect_equal(gnrs_similarity_pair("T'bilisi", "Tbilisi"), round(6 / 11, 7))
  # Whitespace is not a trigram, so padding does not change the score
  expect_equal(gnrs_similarity_pair("  Mexico  ", "Mexico"), 1)
  # Below the default threshold, as on the service
  expect_lt(gnrs_similarity_pair("Sonra", "Sonora"), 0.5)
  # Nothing to compare scores zero rather than failing
  expect_equal(gnrs_similarity_pair("", "Mexico"), 0)
  expect_equal(gnrs_similarity_pair("-", "-"), 0)
})

test_that("the inverted index scores a query against every name in a scope", {
  index <- gnrs_trigram_index(c("United States", "Mexico", "Netherlands", ""))
  s <- gnrs_similarity(gnrs_trigrams("Untied States")[[1]], index)
  expect_equal(length(s), 4)
  expect_equal(round(s[1], 4), 0.5556)
  expect_equal(s[2:4], c(0, 0, 0))
  # An empty query, or a query sharing no trigram, scores zero everywhere
  expect_equal(gnrs_similarity(character(0), index), c(0, 0, 0, 0))
  expect_equal(gnrs_similarity(gnrs_trigrams("zzzz")[[1]], index), c(0, 0, 0, 0))
  expect_equal(gnrs_similarity(gnrs_trigrams("x")[[1]], gnrs_trigram_index(character(0))), numeric(0))
})

test_that("scores are stored as NUMERIC(4,2) is stored", {
  # Two thirds is 0.67, and halves round away from zero
  expect_equal(gnrs_numeric2(c(2 / 3, 0.675, 0.5, 5 / 9, 1 / 14, NA)), c(0.67, 0.68, 0.5, 0.56, 0.07, NA))
  expect_equal(gnrs_numeric2(numeric(0)), numeric(0))
})

test_that("wildcard containment runs both ways and ignores blanks", {
  expect_equal(
    gnrs_either_contains(c("Pima", "Pima County", "Cochise", "", NA), "Pima County, AZ"),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  # The submitted value inside a name
  expect_true(gnrs_either_contains("Hermosillo", "ermosill"))
  # Case-sensitive, as LIKE is
  expect_false(gnrs_either_contains("Hermosillo", "HERMOSILLO"))
  expect_equal(gnrs_either_contains(c("a", "b"), ""), c(FALSE, FALSE))
})

test_that("short codes are the last segment of the full ones", {
  expect_equal(gnrs_last_segment(c("US.AZ.019", "CA-BC", "US.AZ", "XX", NA, "")), c("019", "BC", "AZ", "XX", NA, NA))
})
