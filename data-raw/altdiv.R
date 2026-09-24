# ---------------------------------------------------------------------------
# data-raw/altdiv.R
#
# Generates the curated tables of ALTERNATIVE and SUPERSEDED sub-national
# divisions, which ship with the package in inst/extdata/:
#
#   altdiv_units.csv   one row per unit: which system it belongs to, its country,
#                      and, for superseded units, the dates it existed
#   altdiv_names.csv   the names records use for it; match = "exact" or "regex"
#   altdiv_extent.csv  the GADM units a unit covers, where that is known
#
# Why this exists. GNRS resolves a declared state or county against GADM. Large
# parts of the record stream declare divisions that are not GADM divisions at all:
#
#   Sweden   landskap (provinces) and lappmarker are a parallel traditional system;
#            a landskap spans several lan and vice versa. 53.6% of Swedish records
#            with a declared state use one (Smaland 3.7M records, Uppland 3.0M,
#            Vastergotland 2.8M).
#   Britain  the declared county is a Watsonian vice-county, the botanical
#            recording unit, frozen at 1852 boundaries: 99.7% of British records
#            with a declared county, written "VC57 Derbyshire".
#   Norway   the declared county is post-reform (Viken, Innlandet, Vestland,
#            Trondelag); GADM 4.1 still carries the pre-2018 counties.
#
# Without this, GNRS matches such a name to the nearest-looking GADM unit - a
# landskap onto a lan - and the coordinate then falls in a different unit, so the
# record looks geo-invalid when it is correctly georeferenced and correctly
# labelled in its own system. Measured on the garden pipeline, that mislabelled
# 19.8M records as invalid, 70% of them from these three countries.
#
# A unit with a known EXTENT can be validated properly: the coordinate must fall in
# one of the GADM units the declared unit covers. A unit with no extent yet is
# UNVERIFIABLE - recognised, never called invalid.
#
# Extents shipped here are only those that are exactly determined. Swedish
# landskap and lappmarker and the British vice-counties need their boundaries,
# which is a separate sourcing and licensing job (see the GNRS issue).
# ---------------------------------------------------------------------------
suppressMessages({library(data.table); library(nanoparquet)})

OUT <- file.path("C:/Users/bmaitner/Desktop/current_projects/RGNRS", "inst", "extdata")
GADM <- "C:/Users/bmaitner/Desktop/current_projects/garden_variety_traits/data/services_cache/gadmindex-units.gz.parquet"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
D <- function(x) as.Date(x)
units <- list(); names_ <- list(); extent <- list()

add <- function(system, entity_key, country_iso, name, kind, valid_from = NA, valid_to = NA,
                note = NA_character_, alt = character(0), gids = character(0), level = NA_integer_,
                match = "exact", register_name = TRUE) {
  units[[length(units) + 1]] <<- data.table(system, entity_key, country_iso, name, kind,
                                            valid_from = D(valid_from), valid_to = D(valid_to), note)
  # the unit's own name is always an exact lookup; only the alternates carry `match`,
  # so a regex system ("^VC ?[0-9]+") does not register its label as a pattern. A system
  # whose label nobody writes in a record (the vice-counties) supplies alternates only.
  if (match == "exact" && register_name) {
    names_[[length(names_) + 1]] <<- data.table(entity_key, name = name, match = "exact")
  }
  if (length(alt)) {
    names_[[length(names_) + 1]] <<- data.table(entity_key, name = alt, match = match)
  }
  if (length(gids)) {
    extent[[length(extent) + 1]] <<- data.table(entity_key, level = as.integer(level), gid = gids)
  }
}

# ---- Norway: counties superseded by the 2018 and 2020 reforms -------------------
# GADM 4.1 carries the 19 pre-2018 counties. The 2020 reform merged them into 11;
# on 2024-01-01 three of those were dissolved and their predecessors re-established,
# so those names are period-specific. Oslo, Nordland, Rogaland and More og Romsdal
# were unchanged and need no entry: they match GADM directly.
n <- function(key, name, from, to, gids, note) {
  add("no-fylke", key, "NO", name, "superseded", from, to, note, gids = gids, level = 1L)
}
n("NO-TRONDELAG", "Tr\u00f8ndelag", "2018-01-01", NA,
  c("NOR.9_1", "NOR.15_1"), "2018 merger of Nord-Tr\u00f8ndelag and S\u00f8r-Tr\u00f8ndelag")
n("NO-VIKEN", "Viken", "2020-01-01", "2023-12-31",
  c("NOR.1_1", "NOR.4_1", "NOR.2_1"), "Akershus, Buskerud and \u00d8stfold; dissolved 2024-01-01")
n("NO-INNLANDET", "Innlandet", "2020-01-01", NA,
  c("NOR.6_1", "NOR.11_1"), "Hedmark and Oppland")
n("NO-VESTLAND", "Vestland", "2020-01-01", NA,
  c("NOR.7_1", "NOR.14_1"), "Hordaland and Sogn og Fjordane")
n("NO-VESTFOLD-TELEMARK", "Vestfold og Telemark", "2020-01-01", "2023-12-31",
  c("NOR.19_1", "NOR.16_1"), "Vestfold and Telemark; dissolved 2024-01-01")
n("NO-TROMS-FINNMARK", "Troms og Finnmark", "2020-01-01", "2023-12-31",
  c("NOR.17_1", "NOR.5_1"), "Troms and Finnmark; dissolved 2024-01-01")
n("NO-AGDER", "Agder", "2020-01-01", NA,
  c("NOR.3_1", "NOR.18_1"), "Aust-Agder and Vest-Agder")

# ---- Sweden: the lan under the names records actually use -----------------------
# GADM stores the bare name ("Norrbotten"); records write "Norrbottens lan" or
# "Norrbotten lan". These are the SAME unit, so they are aliases with a one-unit
# extent: recognising them ADDS validation rather than suppressing it. Both the
# genitive and the plain form are generated; whichever does not occur simply never
# matches.
#
# The BARE name is registered too, because the GNRS reference names these units
# inconsistently - "Norrbotten" but "Vastmanlands lan", "Vestfold fylke" - so a record
# saying "Skane" does not match the reference's "Skane lan" exactly and would
# otherwise fall through to the landskap of the same name and lose a unit it could
# have been checked against. Where a name matches both, the matcher prefers the entry
# that has an extent, which is this one.
gadm <- as.data.table(nanoparquet::read_parquet(GADM))
lan <- unique(gadm[gid_0 == "SWE", .(gid_1, name_1)])
for (i in seq_len(nrow(lan))) {
  add("se-lan-alias", paste0("SE-LAN-", lan$gid_1[i]), "SE", lan$name_1[i], "alias",
      note = "the same GADM unit under the name records use",
      alt = paste0(lan$name_1[i], c(" l\u00e4n", "s l\u00e4n")),
      gids = lan$gid_1[i], level = 1L)
}

# ---- Sweden: landskap (provinces), a parallel traditional system -----------------
# 25 units. Where a landskap shares its name with a lan (Skane, Gotland, Halland and
# others) the exact GADM match wins and this entry never fires; the ones that matter
# are those with no lan of the same name (Smaland, Uppland, Vastergotland ...).
landskap <- c("Blekinge", "Bohusl\u00e4n", "Dalarna", "Dalsland", "Gotland", "G\u00e4strikland",
              "Halland", "H\u00e4lsingland", "H\u00e4rjedalen", "J\u00e4mtland", "Lappland",
              "Medelpad", "Norrbotten", "N\u00e4rke", "Sk\u00e5ne", "Sm\u00e5land",
              "S\u00f6dermanland", "Uppland", "V\u00e4rmland", "V\u00e4sterbotten",
              "V\u00e4sterg\u00f6tland", "V\u00e4stmanland", "\u00c5ngermanland", "\u00d6land",
              "\u00d6sterg\u00f6tland")
for (x in landskap) {
  add("se-landskap", paste0("SE-LS-", gsub("[^A-Za-z]", "", iconv(x, "UTF-8", "ASCII//TRANSLIT"))),
      "SE", x, "parallel", note = "landskap (province); extent not yet sourced")
}

# ---- Sweden: lappmarker ----------------------------------------------------------
for (x in c("Lule", "Lycksele", "Pite", "Torne", "\u00c5sele")) {
  add("se-lappmark", paste0("SE-LP-", gsub("[^A-Za-z]", "", iconv(x, "UTF-8", "ASCII//TRANSLIT"))),
      "SE", paste(x, "lappmark"), "parallel", note = "lappmark; extent not yet sourced")
}

# ---- Britain and Ireland: Watsonian vice-counties ---------------------------------
# 112 vice-counties in Great Britain and 40 in Ireland, written "VC57 Derbyshire".
# One entry recognises the whole system by the way records write it; the individual
# units are only needed once their boundaries are sourced, which is what would give
# them an extent.
for (cc in c("GB", "IE")) {
  add("gb-vice-county", paste0(cc, "-VC"), cc, "Watsonian vice-county", "parallel",
      note = "botanical recording unit, boundaries frozen at 1852; extent not yet sourced",
      alt = "^VC ?[0-9]+", match = "regex")
}

u <- rbindlist(units); nm <- unique(rbindlist(names_), by = c("entity_key", "name"))
ex <- if (length(extent)) rbindlist(extent) else data.table(entity_key = character(0), level = integer(0), gid = character(0))
stopifnot(!anyDuplicated(u$entity_key), all(ex$entity_key %in% u$entity_key), all(nm$entity_key %in% u$entity_key))
fwrite(u, file.path(OUT, "altdiv_units.csv"))
fwrite(nm, file.path(OUT, "altdiv_names.csv"))
fwrite(ex, file.path(OUT, "altdiv_extent.csv"))
cat(sprintf("units %d (%s) | names %d | extents %d over %d units\n", nrow(u),
            paste(sprintf("%s %d", names(table(u$system)), table(u$system)), collapse = ", "),
            nrow(nm), nrow(ex), uniqueN(ex$entity_key)))
