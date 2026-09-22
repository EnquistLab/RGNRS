context("offline resolution of historical political divisions")

# Against the synthetic reference in helper-local.R plus the history component.
# Nothing derived from CShapes ships (CC BY-NC-SA 4.0; the package is MIT), so the
# history component is assembled from the shipped curation and a CShapes copy built
# into the test cache from the cshapes package - no download, but the package is needed.

dir <- gnrs_test_backbone()
gnrs_test_cshapes(dir)
gnrs_build_history(dir = dir, quiet = TRUE)

test_that("nothing derived from CShapes ships with the package", {
  shipped <- list.files(dirname(gnrs_extdata("history_codes.csv")), pattern = "[.]csv$")
  expect_true(file.exists(file.path(dirname(gnrs_extdata("history_codes.csv")), "SOURCES.md")))
  expect_setequal(shipped, c("history_codes.csv", "history_current_entities.csv",
                             "history_curated_entities.csv", "history_curated_lineage.csv",
                             "history_curated_names.csv", "history_curated_periods.csv",
                             "history_geonames_names.csv"))
  # the tables that were CShapes-derived must not come back
  expect_false(any(c("cshapes_entity_periods.csv", "history_entities.csv",
                     "history_lineage.csv", "history_names.csv") %in% shipped))
  # the only CSH<gwcode> entities in the curation are the ones named by hand
  ent <- utils::read.csv(gnrs_extdata("history_curated_entities.csv"), na.strings = "")
  expect_setequal(grep("^CSH[0-9]+$", ent$entity_key, value = TRUE),
                  c("CSH665", "CSH3", "CSH4", "CSH21", "CSH730"))
})

test_that("assembly at build time reproduces the tables that used to ship", {
  # counts of the inst/extdata tables written by data-raw/history_crosswalk.R before
  # 2026-09-22, which the assembly was checked against row for row
  h <- gnrs_history(dir)
  expect_equal(nrow(h$entities), 326L)
  expect_equal(nrow(h$lineage), 125L)
  expect_equal(nrow(h$periods), 255L)
  expect_equal(sum(h$entities$kind == "historical" & grepl("^CSH[0-9]+$", h$entities$entity_key)), 60L)
  expect_true(any(grepl("^CShapes geometry overlap", h$lineage$source)))
})

test_that("record dates parse from years, ISO dates and Dates", {
  d <- gnrs_parse_record_date(c(1985, 1970))
  expect_equal(d, as.Date(c("1985-07-01", "1970-07-01")))
  # a vector with no ISO dates must not fail: paste0(character(0), "-01") is "-01"
  expect_silent(gnrs_parse_record_date(c("1985", NA)))
  expect_equal(gnrs_parse_record_date(c("1991-12", "2005-03-04", "not a date")),
               as.Date(c("1991-12-01", "2005-03-04", NA)))
  expect_equal(gnrs_parse_record_date(as.Date("2001-01-01")), as.Date("2001-01-01"))
})

test_that("the history component gives every entity a unique identifier", {
  h <- gnrs_history(dir)
  expect_false(anyDuplicated(h$entities$entity_id) > 0)
  synthetic <- is.na(h$entities$geonameid)
  expect_true(all(h$entities$entity_id[synthetic] > gnrs_history_synthetic_base()))
  expect_equal(h$entities$entity_id[h$entities$entity_key == "SUHH"], 8354411L)
  # "NA" is Namibia's code, not a missing value
  expect_true("NA" %in% h$entities$entity_key)
  expect_false(anyNA(h$entities$entity_key))
  expect_true(all(c("entities", "names", "lineage", "periods", "codes", "collisions") %in%
    sub("^history-(.*)\\.gz\\.parquet$", "\\1", list.files(dir, pattern = "^history-"))))
})

test_that("history = 'current' resolves against today's divisions only; 'all' is the default", {
  cur <- gnrs_test_resolve(dir, c("United States", "USSR"), c("Arizona", ""), history = "current")
  expect_equal(cur$country, c("United States", ""))
  expect_false("entity_key" %in% names(cur))
  def <- gnrs_test_resolve(dir, c("United States", "USSR"), c("Arizona", ""))
  all <- gnrs_test_resolve(dir, c("United States", "USSR"), c("Arizona", ""), history = "all")
  expect_identical(def, all)
  expect_equal(def$entity_key, c("US", "SUHH"))
  # the web-service columns agree with "current" wherever no former country is involved
  expect_identical(def[1, names(cur)], cur[1, ])
})

test_that("former countries resolve with history = 'all'", {
  r <- gnrs_test_resolve(dir, c("USSR", "Yugoslavia", "Czechoslovakia (former)", "United States"),
                         history = "all")
  expect_equal(r$entity_key, c("SUHH", "YUCS", "CSHH", "US"))
  expect_equal(r$is_historical, c(TRUE, TRUE, TRUE, FALSE))
  expect_match(r$successors[1], "(^|;)RU(;|$)")
  expect_match(r$successors[1], "(^|;)LT(;|$)")
  # the lineage is followed through Serbia to Kosovo, which seceded from it
  expect_match(r$successors[2], "(^|;)RS(;|$)")
  expect_match(r$successors[2], "(^|;)XK(;|$)")
  expect_equal(r$successors[4], "")
  # "Czechoslovakia (former)" is not an exact alternate name (case differs): fuzzy
  expect_match(r$match_method_country[3], "^fuzzy")
})

test_that("names of synthetic CShapes entities match exactly only", {
  r <- gnrs_test_resolve(dir, c("Northeastern Rhodesia", "Northeastern Rhodesie"), history = "all")
  expect_equal(r$entity_key[1], "CSH5518")
  expect_false(identical(r$entity_key[2], "CSH5518"))
})

test_that("history = 'at_date' keeps a former country only while it existed", {
  df <- data.frame(user_id = 1:3, country = "USSR", state_province = "", county_parish = "",
                   date = c(1985, 1992, 2005), stringsAsFactors = FALSE)
  r <- GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE, history = "at_date")
  # 1992 is within the default one-year tolerance of 1991-12-25
  expect_equal(r$entity_key, c("SUHH", "SUHH", ""))
  expect_match(r$date_check[1], "valid at record date")
  expect_match(r$date_check[3], "outside its validity")
  r0 <- GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE, history = "at_date",
                   tolerance_years = 0)
  expect_equal(r0$entity_key, c("SUHH", "", ""))
})

test_that("a name marked as former is accepted after the division ended, not before it began", {
  expect_equal(gnrs_is_former_label(c("Former USSR", "EX-USSR", "Ex USSR", "USSR (former)",
                                      "Yugoslavia (Former)", "ehemalige DDR", "USSR", "Exeter", "Texas")),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  df <- data.frame(user_id = 1:3, country = c("Former USSR", "Ex-USSR", "Former USSR"),
                   state_province = "", county_parish = "", date = c(1995, 2010, 1900),
                   stringsAsFactors = FALSE)
  r <- GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE, history = "at_date")
  expect_equal(r$entity_key, c("SUHH", "SUHH", ""))
  expect_match(r$date_check[1], "named as former")
  expect_match(r$date_check[3], "outside its validity")
})

test_that("tolerance_years must be a single non-negative number", {
  df <- data.frame(user_id = 1, country = "USSR", state_province = "", county_parish = "", date = "1970")
  expect_error(GNRS_local(df, dir = dir, history = "at_date", tolerance_years = NA, quiet = TRUE), "tolerance_years")
  expect_error(GNRS_local(df, dir = dir, history = "at_date", tolerance_years = c(1, 2), quiet = TRUE), "tolerance_years")
  expect_error(GNRS_local(df, dir = dir, history = "at_date", tolerance_years = -1, quiet = TRUE), "tolerance_years")
})

test_that("a component counts as built only when every one of its files is there", {
  expect_true(gnrs_is_built("history", dir))
  expect_true(gnrs_is_built("cshapes", dir))
  partial <- file.path(tempdir(), "gnrs-partial-history")
  unlink(partial, recursive = TRUE)
  dir.create(partial)
  file.copy(gnrs_history_path("entities", dir), gnrs_history_path("entities", partial))
  file.copy(gnrs_cshapes_versions_path(dir), gnrs_cshapes_versions_path(partial))
  expect_false(gnrs_is_built("history", partial))
  expect_false(gnrs_is_built("cshapes", partial))
})

test_that("without alternate names a former country is found by its own name only", {
  with_alt <- gnrs_test_resolve(dir, c("USSR", "Union of Soviet Socialist Republics"))
  expect_equal(with_alt$entity_key, c("SUHH", "SUHH"))
  without <- gnrs_test_resolve(dir, c("USSR", "Union of Soviet Socialist Republics"), alternate_names = FALSE)
  expect_false(without$entity_key[1] == "SUHH")
  expect_equal(without$entity_key[2], "SUHH")
})

test_that("the history arguments come after quiet, so positional callers still work", {
  df <- data.frame(user_id = 1, country = "Mexico", state_province = "", county_parish = "")
  r <- GNRS_local(df, 0.5, TRUE, dir, FALSE, TRUE)
  expect_equal(r$country, "Mexico")
  expect_equal(names(formals(GNRS_local))[6], "quiet")
})
