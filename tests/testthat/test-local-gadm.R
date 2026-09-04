context("the GADM layer")

# The synthetic GADM table in helper-local.R plays a newer release against the
# synthetic snapshot: identifiers kept, changed and new, and one country gone.

test_that("GADM divisions link by identifier, HASC and name, and the rest are added", {
  dir <- gnrs_test_backbone()
  country <- as.data.frame(nanoparquet::read_parquet(gnrs_snapshot_path("country", dir)))
  state <- as.data.frame(nanoparquet::read_parquet(gnrs_snapshot_path("state_province", dir)))
  county <- as.data.frame(nanoparquet::read_parquet(gnrs_snapshot_path("county_parish", dir)))

  alt <- as.data.frame(nanoparquet::read_parquet(gnrs_altnames_path(dir)))
  applied <- gnrs_apply_gadm(country, state, county, gnrs_test_gadm(), altnames = alt)
  k <- applied$counts
  expect_equal(unlist(k), c(
    country_linked = 2, country_added = 1, state_linked = 5, state_added = 3,
    county_linked = 8, county_added = 2
  ))
  expect_equal(applied$steps$state, c(identifier = 1, hasc = 2, name = 2, alternate_name = 0, reduced_name = 0, truncated_name = 0, contained_name = 0, spatial_veto = 0))
  expect_equal(applied$steps$county, c(statistical_code = 0, identifier = 3, hasc = 0, name = 2, alternate_name = 1, reduced_name = 1, truncated_name = 0, contained_name = 1, spatial_veto = 0))

  st <- applied$state
  # Arizona linked by identifier, Sonora by HASC after its identifier changed
  expect_equal(st$gid_1[st$state_province == "Arizona"], "USA.3_1")
  expect_equal(st$gid_1[st$state_province == "Estado de Sonora"], "MEX.26_2")
  # Georgia's old identifier now belongs to Utah: the HASC codes disagree, so
  # the identifier is not believed; Georgia links to its new one by HASC and
  # Utah is added
  expect_equal(st$gid_1[st$state_province == "Georgia"], "USA.12_2")
  expect_false(st$is_geoname[st$state_province == "Utah"])
  # Chihuahua's recorded identifier and HASC are Coahuila's and the other way
  # round; both codes agree with the wrong division, but the names disagree
  # and each name is borne by another GADM row, so the names decide
  expect_equal(st$gid_1[st$state_province == "Estado de Chihuahua"], "MEX.8_1")
  expect_equal(st$gid_1[st$state_province == "Coahuila"], "MEX.6_1")
  # A division absent from this GADM keeps its row but not its identifiers
  expect_true(is.na(st$gid_1[st$state_province == "Testregion"]))
  expect_true(is.na(applied$country$gid_0[applied$country$country == "Testland"]))
  # New divisions are numbered above everything, under their parent
  new_state <- st[st$state_province == "Newstate", ]
  expect_true(new_state$state_province_id > max(county$county_parish_id))
  expect_equal(new_state$country_id, 6252001L)
  expect_false(new_state$is_geoname)
  expect_equal(new_state$hasc_full, "US.NS")
  newland <- applied$country[applied$country$country == "Newland", ]
  expect_true(is.na(newland$iso))
  expect_equal(newland$iso_alpha3, "NEW")
  expect_equal(st$country_id[st$state_province == "Newprov"], newland$country_id)

  ct <- applied$county
  # Pima linked by name once its identifier and HASC had both changed
  expect_equal(ct$gid_2[ct$county_parish == "Pima County"], "USA.3.11_2")
  expect_equal(ct$hasc_2_full[ct$county_parish == "Pima County"], "US.AZ.PM")
  expect_equal(ct$state_province_id[ct$county_parish == "Newcounty"], 5551752L)
  # Cochise's old identifier now belongs to Yuma, a different county: the
  # identifier alone is not believed, Yuma is added and Cochise loses it
  expect_true(is.na(ct$gid_2[ct$county_parish == "Cochise County"]))
  expect_false(ct$is_geoname[ct$county_parish == "Yuma"])
  # Of the two Hermosillo rows the GeoNames one is linked, the duplicate not
  expect_equal(ct$gid_2[ct$county_parish_id == 8583394], "MEX.26.28_1")
  expect_true(is.na(ct$gid_2[ct$county_parish_id == 90000004]))
  # The copy of Huepac under Chihuahua carries Huepac's identifier and HASC,
  # but the GeoNames row under Sonora bears the name, and gets the link
  expect_equal(ct$gid_2[ct$county_parish_id == 8583398], "MEX.26.32_1")
  expect_true(is.na(ct$gid_2[ct$county_parish_id == 90000005]))
  # "Municipio de Ures" and "Ures" reduce to the same name
  expect_equal(ct$gid_2[ct$county_parish == "Municipio de Ures"], "MEX.26.70_2")
  # "Caborca" is whole words of "Heroica Caborca", and nothing else could be
  expect_equal(ct$gid_2[ct$county_parish == "Heroica Caborca"], "MEX.26.17_2")
  # "Nogales (Heroica)" is "Heroica Nogales" with the words the other way round
  expect_equal(ct$gid_2[ct$county_parish == "Heroica Nogales"], "MEX.26.43_2")
  # Villa Hidalgo's identifier points at "Hidalgo", whose name is whole words
  # of its own: believed without the geometry ...
  expect_equal(ct$gid_2[ct$county_parish == "Villa Hidalgo"], "MEX.26.72_1")

  # ... and withdrawn with it, the point being 150 km from that polygon;
  # links whose names agree outright are kept whatever the distance
  applied <- gnrs_apply_gadm(country, state, county, gnrs_test_gadm(), altnames = alt, distances = gnrs_test_distances())
  ct <- applied$county
  expect_equal(applied$steps$county[["spatial_veto"]], 1)
  expect_true(is.na(ct$gid_2[ct$county_parish == "Villa Hidalgo"]))
  expect_true("Hidalgo" %in% ct$county_parish)
  expect_equal(ct$gid_2[ct$county_parish == "Pima County"], "USA.3.11_2")
  expect_equal(applied$counts$county_linked, 7)
  # "Villa Pesqueira" is a GeoNames alternate name of Matape
  expect_equal(ct$gid_2[ct$county_parish == "Mátape"], "MEX.26.69_1")
  expect_false("Villa Pesqueira" %in% ct$county_parish)

  # Without the alternate names Matape is not linked by anything until the
  # statistical code is trusted, which takes more agreeing divisions than
  # this country has
  applied <- gnrs_apply_gadm(country, state, county, gnrs_test_gadm())
  ct <- applied$county
  expect_true(is.na(ct$gid_2[ct$county_parish == "Mátape"]))
  expect_true("Villa Pesqueira" %in% ct$county_parish)

  # With two agreeing divisions enough, Mexico's code is trusted and links
  # Matape to Villa Pesqueira, its current name
  applied <- gnrs_apply_gadm(country, state, county, gnrs_test_gadm(), code_min_n = 2)
  ct <- applied$county
  expect_equal(applied$steps$county, c(statistical_code = 3, identifier = 1, hasc = 0, name = 2, alternate_name = 0, reduced_name = 1, truncated_name = 0, contained_name = 1, spatial_veto = 0))
  expect_equal(applied$steps$county_code_countries, "3996063")
  expect_equal(ct$gid_2[ct$county_parish == "Mátape"], "MEX.26.69_1")
  expect_false("Villa Pesqueira" %in% ct$county_parish)
  expect_true("Villa Pesqueira" %in% applied$names$name[applied$names$id == 3980890])

  # GADM's own names, their accent-stripped forms and its alternate names
  nm <- applied$names
  az <- nm$name[nm$level == "state_province" & nm$id == 5551752]
  expect_true(all(c("Arizona", "AZ", "Arizona State") %in% az))
  expect_true("Nueva" %in% nm$name[nm$level == "county_parish"])
  expect_true("United States" %in% nm$name[nm$level == "country" & nm$id == 6252001])
})

test_that("a built layer changes what GNRS_local() reports, and can be removed again", {
  dir <- gnrs_test_backbone()
  nanoparquet::write_parquet(gnrs_test_gadm(), gnrs_gadm_path(dir), compression = "gzip")
  saveRDS(list(source = "gadm", version = "test", downloaded = "2024-06-01"), gnrs_provenance_path("gadm", dir))
  gnrs_finalize_reference(dir, quiet = TRUE)
  gnrs_assemble_names(dir, quiet = TRUE)

  r <- gnrs_test_resolve(dir, c("United States", "United States", "Newland", "Mexico"), c("Arizona", "Newstate", "Newprov", "Sonora"), c("Pima", "", "", "Hermosillo"))
  expect_equal(r$gid_2[1], "USA.3.11_2")
  expect_equal(r$state_province[2], "Newstate")
  expect_equal(r$match_method_state_province[2], "exact name")
  expect_equal(r$country[3], "Newland")
  expect_equal(r$gid_1[4], "MEX.26_2")
  expect_equal(r$country_id[1], "6252001")
  # A GADM alternate name reaches the wildcard step
  expect_equal(gnrs_test_resolve(dir, "United States", "Arizona State")$state_province, "Arizona")

  s <- suppressMessages(GNRS_local_status(dir))
  expect_true(s$built[s$source == "gadm"])
  expect_equal(s$version[s$source == "gadm"], "test")
  cit <- GNRS_local_citations(dir, quiet = TRUE)
  expect_true(any(grepl("GADM \\(2024\\)", cit$citation)))

  expect_message(expect_true(GNRS_local_remove(dir, sources = "gadm", ask = FALSE)), "Removed")
  expect_false(gnrs_is_built("gadm", dir))
  r <- gnrs_test_resolve(dir, c("United States", "United States"), c("Arizona", "Newstate"))
  expect_equal(r$gid_1[1], "USA.3_1")
  expect_equal(r$state_province[2], "")
})

test_that("GADM attributes are read from a GeoPackage without the geometry", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  path <- tempfile(fileext = ".gpkg")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  # The world GeoPackage has one row per lowest-level area, with every level
  # above it in columns; a level a country lacks is "NA"
  DBI::dbWriteTable(con, "gadm_410", data.frame(
    GID_0 = c("JAM", "JAM", "JAM", "USA"), NAME_0 = c("Jamaica", "Jamaica", "Jamaica", "United States"),
    VARNAME_0 = "NA",
    GID_1 = c("JAM.1_1", "JAM.2_1", "JAM.2_1", "USA.3_1"), NAME_1 = c("Clarendon", "Hanover", "Hanover", "Arizona"),
    VARNAME_1 = c("NA", "Han|Hanover Parish", "Han|Hanover Parish", "AZ"), NL_NAME_1 = "NA",
    TYPE_1 = "Parish", ENGTYPE_1 = "Parish", CC_1 = "NA", HASC_1 = c("JM.CL", "JM.HA", "JM.HA", "US.AZ"),
    ISO_1 = c("JM-13", "NA", "NA", "US-AZ"),
    GID_2 = c("NA", "NA", "NA", "USA.3.11_1"), NAME_2 = c("NA", "NA", "NA", "Pima"), VARNAME_2 = "NA",
    NL_NAME_2 = "NA", TYPE_2 = c("NA", "NA", "NA", "County"), ENGTYPE_2 = c("NA", "NA", "NA", "County"),
    CC_2 = c("NA", "NA", "NA", "019"), HASC_2 = c("NA", "NA", "NA", "US.AZ.PM"),
    geom = I(list(raw(3), raw(3), raw(3), raw(3))),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "gpkg_contents", data.frame(table_name = "gadm_410"))
  DBI::dbDisconnect(con)

  g <- gnrs_read_gadm_gpkg(path)
  expect_equal(g$level, c(0L, 0L, 1L, 1L, 1L, 2L))
  expect_equal(g$name, c("Jamaica", "United States", "Clarendon", "Hanover", "Arizona", "Pima"))
  expect_equal(g$varname[g$name == "Hanover"], "Han|Hanover Parish")
  expect_true(is.na(g$varname[g$name == "Clarendon"]))
  expect_equal(g$hasc[g$level == 2L], "US.AZ.PM")
  expect_equal(g$cc[g$level == 2L], "019")
  expect_true(all(is.na(g$cc[g$level < 2L])))
  expect_equal(g$iso[g$name == "Arizona"], "US-AZ")
  expect_equal(gnrs_gadm_country_name(c("UnitedStates", "SouthAfrica", "Jamaica", "United States", NA)),
    c("United States", "South Africa", "Jamaica", "United States", NA))
})

test_that("the links are measured against the GeoPackage geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("RSQLite")
  dir <- gnrs_test_backbone()
  nanoparquet::write_parquet(gnrs_test_gadm(), gnrs_gadm_path(dir), compression = "gzip")
  saveRDS(list(source = "gadm", version = "test", downloaded = "2024-06-01"), gnrs_provenance_path("gadm", dir))
  gnrs_finalize_reference(dir, quiet = TRUE)
  # Pima's point inside its polygon, Cochise's point a degree east of Yuma's
  # polygon (which carries its old identifier), Arizona's point inside
  nanoparquet::write_parquet(
    data.frame(geonameid = c(5308878L, 5285106L, 5551752L), lat = c(32.1, 32.1, 32.5), lon = c(-111.5, -109.5, -111.5)),
    gnrs_points_path(dir), compression = "gzip"
  )
  square <- function(x0, y0, x1, y1) sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
  gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(
    sf::st_sf(
      GID_0 = "USA", GID_1 = "USA.3_1", GID_2 = c("USA.3.11_2", "USA.3.2_1", "USA.3.99_1"),
      geom = sf::st_sfc(square(-112, 31, -111, 33), square(-111, 31, -110.5, 33), square(-110.5, 31, -109, 33), crs = 4326)
    ),
    gpkg, layer = "gadm_410", quiet = TRUE
  )
  d <- gnrs_measure_links(gpkg, dir, quiet = TRUE)
  expect_equal(d$km[d$geonameid == 5308878], 0)
  expect_equal(d$km[d$geonameid == 5551752], 0)
  # Cochise keeps no link in this GADM (its identifier belongs to Yuma), so
  # it is not measured; Villa Hidalgo has no coordinates
  expect_false(5285106 %in% d$geonameid)
  expect_equal(sort(unique(d$level)), c("county_parish", "state_province"))
})
