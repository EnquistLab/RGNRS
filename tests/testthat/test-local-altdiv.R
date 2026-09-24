context("alternative and superseded sub-national divisions")

# Built from curation shipped with the package; nothing is downloaded and nothing is
# derived from another component, so this needs no backbone.

dir <- file.path(tempdir(), "gnrs-altdiv-cache")
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
gnrs_build_altdiv(dir = dir, quiet = TRUE)

test_that("the component builds and reports what it knows", {
  expect_true(gnrs_is_built("altdiv", dir))
  a <- gnrs_altdiv(dir)
  expect_true(all(c("units", "names", "extent") %in% names(a)))
  expect_false(anyDuplicated(a$units$entity_key) > 0)
  expect_true(all(a$extent$entity_key %in% a$units$entity_key))
  expect_true(all(a$names$entity_key %in% a$units$entity_key))
  # a unit has an extent exactly when its key appears in the extent table
  expect_equal(sort(a$units$entity_key[a$units$extent_known]), sort(unique(a$extent$entity_key)))
})

test_that("a division that IS a GADM unit is not diverted to another system", {
  # nothing here belongs to another division system, and no alias is registered for
  # these countries: Norwegian counties are covered only where a reform superseded them
  m <- gnrs_altdiv_match(c("NO", "US", "GB"), c("Akershus", "California", "Greater London"), dir = dir)
  expect_true(all(is.na(m$entity_key)))
  # a bare lan name IS registered, as an alias of that same lan: the GNRS reference
  # names these units inconsistently ("Norrbotten" but "Vastmanlands lan"), so the bare
  # name has to be recognised. It carries the lan's own extent, and the resolver takes
  # an exact GADM match first in any case.
  m <- gnrs_altdiv_match("SE", "Stockholm", dir = dir)
  expect_equal(m$kind, "alias")
  expect_true(m$extent_known)
  expect_equal(gnrs_altdiv_extent(m$entity_key, dir), "SWE.15_1")
})

test_that("parallel systems are recognised but carry no extent yet", {
  m <- gnrs_altdiv_match(c("SE", "SE", "SE"), c("Uppland", "Småland", "Lule lappmark"), dir = dir)
  expect_equal(m$system, c("se-landskap", "se-landskap", "se-lappmark"))
  expect_equal(m$kind, rep("parallel", 3))
  expect_false(any(m$extent_known))
  expect_equal(gnrs_altdiv_extent(m$entity_key[1], dir), character(0))
})

test_that("vice-counties are matched by the way records write them", {
  m <- gnrs_altdiv_match(rep("GB", 3), c("VC57 Derbyshire", "VC1 West Cornwall", "VC 9 Dorset"), dir = dir)
  expect_equal(unique(m$system), "gb-vice-county")
  # a county that is not a vice-county is left alone
  expect_true(is.na(gnrs_altdiv_match("GB", "Greater London", dir = dir)$entity_key))
})

test_that("a lan under the name records use resolves to that same lan", {
  m <- gnrs_altdiv_match(c("SE", "SE"), c("Norrbottens län", "Norrbotten län"), dir = dir)
  expect_equal(m$kind, c("alias", "alias"))
  expect_true(all(m$extent_known))
  expect_equal(gnrs_altdiv_extent(m$entity_key[1], dir), "SWE.10_1")
})

test_that("superseded counties carry their extent and their period", {
  m <- gnrs_altdiv_match(rep("NO", 3), rep("Viken", 3),
                         dates = c("2021-06-01", "2025-06-01", NA), dir = dir)
  expect_equal(unique(m$entity_key), "NO-VIKEN")
  expect_true(all(m$extent_known))
  expect_setequal(gnrs_altdiv_extent("NO-VIKEN", dir), c("NOR.1_1", "NOR.4_1", "NOR.2_1"))
  # Viken existed 2020-01-01 to 2023-12-31; with no date there is nothing to check
  expect_equal(m$in_period, c(TRUE, FALSE, NA))
  # Innlandet was not dissolved in 2024
  expect_true(gnrs_altdiv_match("NO", "Innlandet", dates = "2025-06-01", dir = dir)$in_period)
})

test_that("extents name GADM units that exist", {
  a <- gnrs_altdiv(dir)
  expect_true(all(grepl("^[A-Z]{3}[.][0-9]+(_[0-9]+)?$", a$extent$gid)))
  expect_true(all(a$extent$level %in% c(1L, 2L)))
})
