context("building the local reference")

test_that("the API lists become the reference tables the SQL expects", {
  dir <- gnrs_test_backbone()
  country <- nanoparquet::read_parquet(gnrs_reference_path("country", dir))
  state <- nanoparquet::read_parquet(gnrs_reference_path("state_province", dir))
  county <- nanoparquet::read_parquet(gnrs_reference_path("county_parish", dir))

  # GADM additions are the identifiers above the boundary set by the first
  # country without an ISO code
  expect_equal(country$is_geoname, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(readRDS(gnrs_provenance_path("gnrs", dir))$geonames_max_id, 90000000L)
  expect_false(state$is_geoname[state$state_province == "Testregion"])

  # State-as-country links, by name
  expect_equal(country$alt_country_id[country$country == "Puerto Rico"], 6252001L)
  expect_true(is.na(country$alt_country_id[country$country == "United States"]))

  # Short codes from full ones; short HASC codes are never derived, since the
  # deployed service does not match on them
  expect_equal(state$state_province_code[state$state_province == "Arizona"], "AZ")
  expect_equal(state$state_province_code2[state$state_province == "Estado de Sonora"], "SON")
  expect_true(all(is.na(state$hasc)))
  expect_equal(state$hasc_full[state$state_province == "Arizona"], "US.AZ")
  expect_equal(county$county_parish_code[county$county_parish == "Pima County"], "019")
  expect_true(all(is.na(county$hasc_2)))

  expect_equal(state$is_countryasstate, state$state_province %in% c("England", "Scotland"))
  expect_equal(county$is_stateascounty, county$state_province_ascii %in% c("England", "Scotland"))
})

test_that("the name table is assembled as the upstream build assembles it", {
  dir <- gnrs_test_backbone()
  names <- nanoparquet::read_parquet(gnrs_names_path(dir))

  us <- names[names$level == "country" & names$id == 6252001, ]
  # GeoNames names and the standard name itself carry the GeoNames type
  expect_true(all(c("USA", "United States") %in% us$name[us$name_type == "original from geonames"]))
  expect_equal(us$name_type[us$name == "US"], "iso code")
  expect_equal(us$name_type[us$name == "USA"], "original from geonames")

  # A GADM-only division gets its names typed "from GADM"
  tl <- names[names$level == "country" & names$id == 90000001, ]
  expect_equal(unique(tl$name_type), "from GADM")
  # and only where the name is absent from the whole table: "Georgia" is a
  # GeoNames alternate name of the US state, so nothing is added for it
  # under the GADM rule, while its own name still carries the GeoNames type
  ga <- names[names$level == "state_province" & names$id == 4197000, ]
  # Its short HASC "GA" is already there as its short ISO code, so the pair
  # is not added twice and no "hasc code" row appears
  expect_equal(sort(unique(ga$name_type)), c("full code2", "full iso code", "original from geonames", "short iso code"))

  # ISO and alternate codes become names of the GeoNames divisions only, and
  # HASC codes never do
  az <- names[names$level == "state_province" & names$id == 5551752, ]
  expect_true(all(c("US.AZ", "AZ", "US-AZ") %in% az$name))
  expect_false("hasc code" %in% names$name_type)
  expect_false("full hasc code" %in% names$name_type)
  tr <- names[names$level == "state_province" & names$id == 90000002, ]
  expect_false("TS.TR" %in% tr$name)

  # The hand-added upstream names only apply where their state exists, which
  # none of them does in the synthetic reference
  expect_equal(nrow(names[names$name == "Lima Province", ]), 0)
})

test_that("unknown sources are refused and the order is fixed", {
  tmp <- file.path(tempdir(), "gnrs-build-args")
  dir.create(tmp, showWarnings = FALSE)
  expect_message(r <- GNRS_local_build("nope", dir = tmp, quiet = TRUE), "Unknown source")
  expect_null(r)
})

test_that("reading the alternate names keeps only what the SQL keeps", {
  tmp <- tempfile(fileext = ".zip")
  txt <- file.path(tempdir(), "alternateNamesV2.txt")
  lines <- c(
    "1\t6252001\ten\tUnited States\t1\t\t\t\t\t",
    "2\t6252001\t\tNo language\t\t\t\t\t\t",
    "3\t6252001\tlink\thttp://example.org\t\t\t\t\t\t",
    "4\t6252001\twkdt\tQ30\t\t\t\t\t\t",
    "5\t6252001\tes\tEstados Unidos\t\t\t\t\t\t",
    "6\t6252001\tfr\tEstados Unidos\t\t\t\t\t\t",
    "7\t9999999\ten\tSomewhere else\t\t\t\t\t\t",
    "8\t6252001\tde\t\t\t\t\t\t\t"
  )
  writeLines(lines, txt, useBytes = TRUE)
  old <- setwd(tempdir())
  utils::zip(tmp, "alternateNamesV2.txt", flags = "-q")
  setwd(old)
  skip_if_not(file.exists(tmp), "zip not available")

  alt <- gnrs_import_altnames(tmp, ids = 6252001L, quiet = TRUE)
  expect_equal(alt$name, c("Estados Unidos", "United States"))
  expect_equal(alt$geonameid, c(6252001L, 6252001L))
})
