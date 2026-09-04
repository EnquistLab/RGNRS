# A small synthetic reference for exercising the offline resolver without a
# download.  It is written in the same layout as a real build, through the
# same functions, so it tests the name-table assembly as well as the cascade.
#
# Identifiers are the real ones where the division is real, so that the tests
# read naturally; "Testland" stands for a country the upstream build added
# from GADM (no ISO code, identifier above the GeoNames boundary).

gnrs_test_backbone <- function(dir = file.path(tempdir(), "gnrs-test-cache")) {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  countries <- data.frame(
    country_id = c("6252001", "4566966", "2635167", "3996063", "90000001"),
    country = c("United States", "Puerto Rico", "United Kingdom", "Mexico", "Testland"),
    iso = c("US", "PR", "GB", "MX", NA),
    iso_alpha3 = c("USA", "PRI", "GBR", "MEX", "TST"),
    fips = c("US", "RQ", "UK", "MX", NA),
    continent_code = c("NA", "NA", "EU", "NA", NA),
    continent = c("North America", "North America", "Europe", "North America", NA),
    gadm_gid_0 = c("USA", "PRI", "GBR", "MEX", "TST"),
    stringsAsFactors = FALSE
  )
  states <- data.frame(
    state_province_id = c("5551752", "4197000", "6269131", "2638360", "3982846", "4566217", "90000002"),
    country_id = c("6252001", "6252001", "2635167", "2635167", "3996063", "4566966", "90000001"),
    country_iso = c("US", "US", "GB", "GB", "MX", "PR", NA),
    country = c("United States", "United States", "United Kingdom", "United Kingdom", "Mexico", "Puerto Rico", "Testland"),
    state_province = c("Arizona", "Georgia", "England", "Scotland", "Estado de Sonora", "Mayagüez", "Testregion"),
    state_province_ascii = c("Arizona", "Georgia", "England", "Scotland", "Estado de Sonora", "Mayaguez", "Testregion"),
    state_province_gnrs = c("Arizona", "Georgia", "England", "Scotland", "Sonora", "Mayaguez", "Testregion"),
    iso_3866_1 = c("US.AZ", "US.GA", "GB.ENG", "GB.SCT", "MX.26", "PR.097", NA),
    iso_3866_1_alt = c("US-AZ", "US-GA", "GB-ENG", "GB-SCT", "MX-SON", NA, NA),
    hasc = c("US.AZ", "US.GA", NA, NA, "MX.SO", NA, "TS.TR"),
    gadm_gid_0 = c("USA", "USA", "GBR", "GBR", "MEX", "PRI", "TST"),
    gadm_gid_1 = c("USA.3_1", "USA.11_1", "GBR.1_1", "GBR.3_1", "MEX.26_1", "PRI.50_1", "TST.1_1"),
    stringsAsFactors = FALSE
  )
  counties <- data.frame(
    county_parish_id = c("5308878", "5285106", "2651292", "2649469", "8583394", "8583398", "90000003"),
    country_id = c("6252001", "6252001", "2635167", "2635167", "3996063", "3996063", "90000001"),
    country = c("United States", "United States", "United Kingdom", "United Kingdom", "Mexico", "Mexico", "Testland"),
    country_iso = c("US", "US", "GB", "GB", "MX", "MX", NA),
    state_province_id = c("5551752", "5551752", "6269131", "2638360", "3982846", "3982846", "90000002"),
    state_province_ascii = c("Arizona", "Arizona", "England", "Scotland", "Estado de Sonora", "Estado de Sonora", "Testregion"),
    county_parish = c("Pima County", "Cochise County", "Devon", "Fife", "Hermosillo", "Huépac", "Testshire"),
    county_parish_ascii = c("Pima County", "Cochise County", "Devon", "Fife", "Hermosillo", "Huepac", "Testshire"),
    county_parish_gnrs = c("Pima", "Cochise", "Devon", "Fife", "Hermosillo", "Huepac", "Testshire"),
    iso_3166_2 = c("US.AZ.019", "US.AZ.003", "GB.ENG.D4", "GB.SCT.V1", "MX.26.030", "MX.26.034", NA),
    iso_3166_2_alt = c(NA, NA, "GB-ENG-DEV", "GB-SCT-FIF", "MX-SON-030", "MX-SON-034", NA),
    hasc2 = c(NA, NA, NA, NA, NA, NA, "TS.TR.TS"),
    gadm_gid_0 = c("USA", "USA", "GBR", "GBR", "MEX", "MEX", "TST"),
    gadm_gid_1 = c("USA.3_1", "USA.3_1", "GBR.1_1", "GBR.3_1", "MEX.26_1", "MEX.26_1", "TST.1_1"),
    gadm_gid_2 = c("USA.3.11_1", "USA.3.2_1", "GBR.1.26_1", "GBR.3.15_1", "MEX.26.28_1", "MEX.26.32_1", "TST.1.1_1"),
    stringsAsFactors = FALSE
  )
  tables <- gnrs_import_reference(countries, states, counties)
  nanoparquet::write_parquet(tables$country, gnrs_reference_path("country", dir), compression = "gzip")
  nanoparquet::write_parquet(tables$state, gnrs_reference_path("state_province", dir), compression = "gzip")
  nanoparquet::write_parquet(tables$county, gnrs_reference_path("county_parish", dir), compression = "gzip")
  saveRDS(
    list(
      source = "gnrs", version = "database test (2024-01-01), code test",
      downloaded = "2024-01-01", geonames_max_id = tables$geonames_max_id
    ),
    gnrs_provenance_path("gnrs", dir)
  )

  alt <- data.frame(
    geonameid = c(
      6252001L, 6252001L, 6252001L, 6252001L, 6252001L,
      4566966L, 2635167L, 2635167L, 2635167L, 3996063L, 3996063L,
      6269131L, 2638360L, 3982846L, 5308878L, 8583394L, 4197000L
    ),
    name = c(
      "USA", "U.S.A.", "United States of America", "America", "Estados Unidos",
      "Porto Rico", "UK", "Great Britain", "Reino Unido", "México", "Estados Unidos Mexicanos",
      "Inglaterra", "Escocia", "Sonora", "Pima", "Hermosillo Municipality", "Georgia"
    ),
    stringsAsFactors = FALSE
  )
  nanoparquet::write_parquet(alt, gnrs_altnames_path(dir), compression = "gzip")
  saveRDS(
    list(source = "geonames", version = "2024-01-01", downloaded = "2024-01-01", n_names = nrow(alt)),
    gnrs_provenance_path("geonames", dir)
  )

  gnrs_assemble_names(dir, quiet = TRUE)
  gnrs_forget_backbone()
  dir
}

# Submit rows the way a user would, against the test backbone
gnrs_test_resolve <- function(dir, country, state = "", county = "", ...) {
  df <- data.frame(
    user_id = seq_along(country), country = country,
    state_province = state, county_parish = county, stringsAsFactors = FALSE
  )
  GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE, ...)
}
