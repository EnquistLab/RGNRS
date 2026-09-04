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
    state_province_id = c("5551752", "4197000", "6269131", "2638360", "3982846", "4014336", "4013514", "4566217", "90000002"),
    country_id = c("6252001", "6252001", "2635167", "2635167", "3996063", "3996063", "3996063", "4566966", "90000001"),
    country_iso = c("US", "US", "GB", "GB", "MX", "MX", "MX", "PR", NA),
    country = c("United States", "United States", "United Kingdom", "United Kingdom", "Mexico", "Mexico", "Mexico", "Puerto Rico", "Testland"),
    state_province = c("Arizona", "Georgia", "England", "Scotland", "Estado de Sonora", "Estado de Chihuahua", "Coahuila", "Mayagüez", "Testregion"),
    state_province_ascii = c("Arizona", "Georgia", "England", "Scotland", "Estado de Sonora", "Estado de Chihuahua", "Coahuila", "Mayaguez", "Testregion"),
    state_province_gnrs = c("Arizona", "Georgia", "England", "Scotland", "Sonora", "Chihuahua", "Coahuila", "Mayaguez", "Testregion"),
    iso_3866_1 = c("US.AZ", "US.GA", "GB.ENG", "GB.SCT", "MX.26", "MX.08", "MX.05", "PR.097", NA),
    iso_3866_1_alt = c("US-AZ", "US-GA", "GB-ENG", "GB-SCT", "MX-SON", "MX-CHH", "MX-COA", NA, NA),
    hasc = c("US.AZ", "US.GA", NA, NA, "MX.SO", "MX.CO", "MX.CH", NA, "TS.TR"),
    gadm_gid_0 = c("USA", "USA", "GBR", "GBR", "MEX", "MEX", "MEX", "PRI", "TST"),
    gadm_gid_1 = c("USA.3_1", "USA.11_1", "GBR.1_1", "GBR.3_1", "MEX.26_1", "MEX.6_1", "MEX.8_1", "PRI.50_1", "TST.1_1"),
    stringsAsFactors = FALSE
  )
  # The service has Chihuahua's and Coahuila's identifiers and HASC codes
  # the wrong way round (as it has Guyana's regions).
  # Sonora has two rows for Hermosillo, as the service's tables have for many
  # divisions (its build added the GADM name beside the GeoNames row), and a
  # copy of Huepac filed under Chihuahua with Huepac's identifier and HASC
  # code, as the service has Napo's cantons filed under Tungurahua; Nogales
  # carries its honorific in front, where GADM has it in brackets behind;
  # Villa Hidalgo's recorded identifier is that of a GADM "Hidalgo" 150 km
  # away, which only the geometry can tell; Ures
  # carries its type word; Matape is the municipality GADM now calls Villa
  # Pesqueira, the two sharing only the statistical code; Caborca's GeoNames
  # name carries its honorific
  counties <- data.frame(
    county_parish_id = c("5308878", "5285106", "2651292", "2649469", "8583394", "8583398", "3980760", "3980890", "3981225", "3994469", "3980199", "90000004", "90000005", "90000003"),
    country_id = c("6252001", "6252001", "2635167", "2635167", "3996063", "3996063", "3996063", "3996063", "3996063", "3996063", "3996063", "3996063", "3996063", "90000001"),
    country = c("United States", "United States", "United Kingdom", "United Kingdom", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Mexico", "Testland"),
    country_iso = c("US", "US", "GB", "GB", "MX", "MX", "MX", "MX", "MX", "MX", "MX", "MX", "MX", NA),
    state_province_id = c("5551752", "5551752", "6269131", "2638360", "3982846", "3982846", "3982846", "3982846", "3982846", "3982846", "3982846", "3982846", "4014336", "90000002"),
    state_province_ascii = c("Arizona", "Arizona", "England", "Scotland", "Estado de Sonora", "Estado de Sonora", "Estado de Sonora", "Estado de Sonora", "Estado de Sonora", "Estado de Sonora", "Estado de Sonora", "Estado de Sonora", "Estado de Chihuahua", "Testregion"),
    county_parish = c("Pima County", "Cochise County", "Devon", "Fife", "Hermosillo", "Huépac", "Municipio de Ures", "Mátape", "Heroica Caborca", "Heroica Nogales", "Villa Hidalgo", "Hermosillo", "Huepac", "Testshire"),
    county_parish_ascii = c("Pima County", "Cochise County", "Devon", "Fife", "Hermosillo", "Huepac", "Municipio de Ures", "Matape", "Heroica Caborca", "Heroica Nogales", "Villa Hidalgo", "Hermosillo", "Huepac", "Testshire"),
    county_parish_gnrs = c("Pima", "Cochise", "Devon", "Fife", "Hermosillo", "Huepac", "Municipio de Ures", "Matape", "Heroica Caborca", "Heroica Nogales", "Villa Hidalgo", "Hermosillo", "Huepac", "Testshire"),
    iso_3166_2 = c("US.AZ.019", "US.AZ.003", "GB.ENG.D4", "GB.SCT.V1", "MX.26.030", "MX.26.034", "MX.26.070", "MX.26.008", "MX.26.017", "MX.26.043", "MX.26.072", NA, NA, NA),
    iso_3166_2_alt = c(NA, NA, "GB-ENG-DEV", "GB-SCT-FIF", "MX-SON-030", "MX-SON-034", "MX-SON-070", "MX-SON-008", "MX-SON-017", "MX-SON-043", "MX-SON-072", NA, NA, NA),
    hasc2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "MX.SO.HU", "TS.TR.TS"),
    gadm_gid_0 = c("USA", "USA", "GBR", "GBR", "MEX", "MEX", "MEX", "MEX", "MEX", "MEX", "MEX", "MEX", "MEX", "TST"),
    gadm_gid_1 = c("USA.3_1", "USA.3_1", "GBR.1_1", "GBR.3_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "MEX.26_1", "TST.1_1"),
    gadm_gid_2 = c("USA.3.11_1", "USA.3.2_1", "GBR.1.26_1", "GBR.3.15_1", "MEX.26.28_1", "MEX.26.32_1", "MEX.26.70_1", "MEX.26.35_1", "MEX.26.17_1", "MEX.26.43_1", "MEX.26.72_1", "MEX.26.28_1", "MEX.26.32_1", "TST.1.1_1"),
    stringsAsFactors = FALSE
  )
  tables <- gnrs_import_reference(countries, states, counties)
  nanoparquet::write_parquet(tables$country, gnrs_snapshot_path("country", dir), compression = "gzip")
  nanoparquet::write_parquet(tables$state, gnrs_snapshot_path("state_province", dir), compression = "gzip")
  nanoparquet::write_parquet(tables$county, gnrs_snapshot_path("county_parish", dir), compression = "gzip")
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
      6269131L, 2638360L, 3982846L, 5308878L, 8583394L, 4197000L, 3980890L, 4013514L
    ),
    name = c(
      "USA", "U.S.A.", "United States of America", "America", "Estados Unidos",
      "Porto Rico", "UK", "Great Britain", "Reino Unido", "México", "Estados Unidos Mexicanos",
      "Inglaterra", "Escocia", "Sonora", "Pima", "Hermosillo Municipality", "Georgia",
      "Villa Pesqueira", "Coahuila de Zaragoza"
    ),
    stringsAsFactors = FALSE
  )
  nanoparquet::write_parquet(alt, gnrs_altnames_path(dir), compression = "gzip")
  saveRDS(
    list(source = "geonames", version = "2024-01-01", downloaded = "2024-01-01", n_names = nrow(alt)),
    gnrs_provenance_path("geonames", dir)
  )

  gnrs_finalize_reference(dir, quiet = TRUE)
  gnrs_assemble_names(dir, quiet = TRUE)
  gnrs_forget_backbone()
  dir
}

# A GADM attribute table for the synthetic reference: one country and its
# divisions as GADM might publish them a few releases on.  Arizona keeps its
# identifier; Sonora's has changed but its HASC has not; Georgia's identifier
# now belongs to Utah, and Georgia has a new one; Pima has neither identifier
# nor HASC and links by name; Cochise's old identifier now belongs to Yuma;
# Ures links once its type word is dropped; Villa Pesqueira is Matape
# renamed, sharing only the statistical code; Caborca is "Heroica Caborca"
# in GeoNames; Coahuila and Chihuahua carry the identifiers and HASC codes
# the service recorded for each other; a new state and a new county have no
# counterpart; and "Testland" is absent from this GADM altogether.
gnrs_test_gadm <- function() {
  data.frame(
    level = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
    gid_0 = c("USA", "MEX", "NEW", "USA", "MEX", "USA", "NEW", "USA", "USA", "MEX", "MEX", "USA", "MEX", "USA", "MEX", "MEX", "MEX", "USA", "MEX", "MEX", "MEX"),
    gid_1 = c(NA, NA, NA, "USA.3_1", "MEX.26_2", "USA.99_1", "NEW.1_1", "USA.11_1", "USA.12_2", "MEX.6_1", "MEX.8_1", "USA.3_1", "MEX.26_2", "USA.3_1", "MEX.26_2", "MEX.26_2", "MEX.26_2", "USA.3_1", "MEX.26_2", "MEX.26_2", "MEX.26_2"),
    gid_2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "USA.3.11_2", "MEX.26.28_1", "USA.3.99_1", "MEX.26.32_1", "MEX.26.70_2", "MEX.26.69_1", "USA.3.2_1", "MEX.26.17_2", "MEX.26.43_2", "MEX.26.72_1"),
    country = c("UnitedStates", "Mexico", "Newland", "UnitedStates", "Mexico", "UnitedStates", "Newland", "UnitedStates", "UnitedStates", "Mexico", "Mexico", "UnitedStates", "Mexico", "UnitedStates", "Mexico", "Mexico", "Mexico", "UnitedStates", "Mexico", "Mexico", "Mexico"),
    name = c("UnitedStates", "Mexico", "Newland", "Arizona", "Sonora", "Newstate", "Newprov", "Utah", "Georgia", "Coahuila", "Chihuahua", "Pima", "Hermosillo", "Newcounty", "Huépac", "Ures", "Villa Pesqueira", "Yuma", "Caborca", "Nogales (Heroica)", "Hidalgo"),
    varname = c(NA, NA, NA, "AZ|Arizona State", "Estado de Sonora", NA, NA, NA, NA, NA, NA, NA, NA, "Nueva", NA, NA, "Mátape", NA, NA, NA, NA),
    nl_name = NA_character_,
    type = c(NA, NA, NA, "State", "Estado", "State", "Province", "State", "State", "Estado", "Estado", "County", "Municipio", "County", "Municipio", "Municipio", "Municipio", "County", "Municipio", "Municipio", "Municipio"),
    engtype = c(NA, NA, NA, "State", "State", "State", "Province", "State", "State", "State", "State", "County", "Municipality", "County", "Municipality", "Municipality", "Municipality", "County", "Municipality", "Municipality", "Municipality"),
    hasc = c(NA, NA, NA, "US.AZ", "MX.SO", "US.NS", "NW.NP", "US.UT", "US.GA", "MX.CO", "MX.CH", "US.AZ.PM", "MX.SO.HE", "US.AZ.NC", "MX.SO.HU", "MX.SO.UR", "MX.SO.VP", "US.AZ.YU", "MX.SO.CB", "MX.SO.NG", "MX.SO.HI"),
    iso = c(NA, NA, NA, "US-AZ", "MX-SON", NA, NA, "US-UT", "US-GA", "MX-COA", "MX-CHH", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    cc = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "019", "030", NA, "034", NA, "008", NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

# Distances a geometry pass would have measured for the synthetic reference:
# every point inside its polygon except Villa Hidalgo's
gnrs_test_distances <- function() {
  data.frame(
    geonameid = c(5308878L, 8583394L, 8583398L, 3980199L, 5551752L),
    level = c("county_parish", "county_parish", "county_parish", "county_parish", "state_province"),
    gid = c("USA.3.11_2", "MEX.26.28_1", "MEX.26.32_1", "MEX.26.72_1", "USA.3_1"),
    km = c(0, 0, 0, 150, 0),
    stringsAsFactors = FALSE
  )
}

# Submit rows the way a user would, against the test backbone
gnrs_test_resolve <- function(dir, country, state = "", county = "", ...) {
  df <- data.frame(
    user_id = seq_along(country), country = country,
    state_province = state, county_parish = county, stringsAsFactors = FALSE
  )
  GNRS_local(df, dir = dir, build_missing = FALSE, quiet = TRUE, ...)
}
