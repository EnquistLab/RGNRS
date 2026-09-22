# ---------------------------------------------------------------------------
# data-raw/history_crosswalk.R
#
# Generates the CURATED inputs of the time-versioned division reference
# (dev_notes/04). They ship with the package in inst/extdata/:
#
#   history_current_entities.csv  snapshot of the service's current countries
#   history_curated_entities.csv  historical entities keyed by ISO 3166-3 alpha-4
#                                 or a synthetic key, with their dates
#   history_curated_periods.csv   curated assignments of CShapes state codes to
#                                 entities over periods (gwcode 365 is the Russian
#                                 Empire, the USSR and the Russian Federation)
#   history_curated_lineage.csv   curated predecessor -> successor links, CLDR
#                                 successor lists resolved
#   history_curated_names.csv     curated names and former names of entities
#   history_geonames_names.csv    GeoNames names of the historical entities
#   history_codes.csv             former country codes and the entity each denotes
#
# NOTHING DERIVED FROM CShapes SHIPS (BM, 2026-09-22). CShapes 2.0 is CC BY-NC-SA
# 4.0 and this package is MIT, so the CShapes-derived parts - periods for every
# uncurated state code, the synthetic CSH<gwcode> entities, their names, the
# coverage check and the geometry-derived lineage - are computed at build time
# from the user's own CShapes copy by gnrs_history_assemble() (R/local_history_assemble.R),
# after GNRS_local_build("cshapes"). This script holds only the curation, which is
# original work, and extracts from redistributable sources.
#
# Modelling rule: a new ENTITY only where the political unit changed (dissolution,
# unification, partition, absorption, a colony that is not today's country). A pure
# rename of a continuing state (Burma -> Myanmar, Zaire -> DR Congo, Dahomey -> Benin,
# Byelorussian SSR -> Belarus, Russian Empire / RSFSR -> Russian Federation outside
# the USSR period) stays ONE entity; its former names and codes go in the names and
# codes tables with their periods.
#
# Sources (read from gvs_ms/data/history_sources/, downloaded 2026-09-16), all
# redistributable in this form:
#   GNRS country table (the service's own identifiers; cached API snapshot; names
#     and GeoNames ids, CC BY 4.0)
#   Unicode CLDR supplementalMetadata.xml territoryAlias (Unicode License v3)
#   Debian iso-codes iso_3166-3.json (LGPL-2.1+)
#   GeoNames allCountries, PCLH historical political entities (CC BY 4.0)
# Every curated decision is written as data below, with a note.
# ---------------------------------------------------------------------------

suppressMessages({library(data.table); library(nanoparquet); library(jsonlite); library(arrow)})

# Run from the package root.  The source downloads (CLDR, GeoNames, ...) live
# wherever GNRS_HISTORY_SOURCES points; the service's country table is read
# from the package's own cache, built with GNRS_local_build().
SRC <- Sys.getenv("GNRS_HISTORY_SOURCES", unset = NA)
if (is.na(SRC)) stop("Set GNRS_HISTORY_SOURCES to the directory holding the source downloads.")
GNRS_CACHE <- Sys.getenv("GNRS_CACHE_DIR", unset = tools::R_user_dir("GNRS", "cache"))
OUT <- file.path("inst", "extdata")
if (!file.exists("DESCRIPTION")) stop("Run this script from the package root.")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
D <- function(x) as.Date(x)

# ---- current entities: the service's country table --------------------------
ct <- as.data.table(read_parquet(file.path(GNRS_CACHE, "gnrs-api-country.gz.parquet")))
cur <- ct[!is.na(iso) & nzchar(iso), .(entity_key = iso, name = country, geonameid = as.character(country_id),
                                        iso3166_1 = iso, iso3166_3 = NA_character_,
                                        valid_from = as.Date(NA), valid_to = as.Date(NA), kind = "current",
                                        note = NA_character_)]
cur <- unique(cur, by = "entity_key")
# shipped as a snapshot, as the entities table always carried it: which uncurated state
# codes map to a current country depends on this list, so it is fixed with the curation
fwrite(cur, file.path(OUT, "history_current_entities.csv"))

# ---- historical entities (curated) -----------------------------------------
# valid_from/valid_to: dates of the political unit, not of any one border.
hist <- rbindlist(list(
  list("SUHH", "Union of Soviet Socialist Republics", "8354411", NA, "SUHH", D("1922-12-30"), D("1991-12-25"), "historical", "Treaty on the Creation of the USSR to dissolution"),
  list("CSHH", "Czechoslovakia", "8505031", NA, "CSHH", D("1918-10-28"), D("1992-12-31"), "historical", "including the Czechoslovak Socialist Republic"),
  list("DDDE", "German Democratic Republic", "8354410", NA, "DDDE", D("1949-10-07"), D("1990-10-02"), "historical", "absorbed by the Federal Republic of Germany"),
  list("YUCS", "Yugoslavia (Kingdom of Serbs, Croats and Slovenes; Kingdom of Yugoslavia; SFR Yugoslavia)", NA, NA, "YUCS", D("1918-12-01"), D("1992-04-26"), "historical", "no GeoNames PCLH; ISO YUCS also covered FR Yugoslavia, modelled separately as CSXX"),
  list("CSXX", "Serbia and Montenegro (Federal Republic of Yugoslavia to 2003)", "8505033", NA, "CSXX", D("1992-04-27"), D("2006-06-03"), "historical", "GeoNames also has FR Yugoslavia as 7500737; one state renamed in 2003"),
  list("YDYE", "People's Democratic Republic of Yemen", "8505034", NA, "YDYE", D("1967-11-30"), D("1990-05-21"), "historical", "merged with the Yemen Arab Republic"),
  list("VNRV", "Republic of Vietnam", "11608491", NA, NA, D("1955-10-26"), D("1975-04-30"), "historical", "synthetic key; absorbed into Vietnam"),
  list("ANHH", "Netherlands Antilles", "8505032", NA, "ANHH", D("1954-12-15"), D("2010-10-09"), "historical", "Aruba separated 1986"),
  list("RUBI", "Ruanda-Urundi", "11612757", NA, NA, D("1922-07-20"), D("1962-06-30"), "historical", "synthetic key; Belgian mandate"),
  list("GEHH", "Gilbert and Ellice Islands", NA, NA, "GEHH", D("1892-05-27"), D("1975-12-31"), "historical", "split into Kiribati and Tuvalu"),
  list("PCHH", "Trust Territory of the Pacific Islands", NA, NA, "PCHH", D("1947-07-18"), D("1994-10-01"), "historical", "divided into FM, MH, MP and PW"),
  list("SKIN", "Sikkim", NA, NA, "SKIN", D("1642-01-01"), D("1975-05-16"), "historical", "absorbed by India"),
  list("PZPA", "Panama Canal Zone", NA, NA, "PZPA", D("1903-11-18"), D("1979-09-30"), "historical", "returned to Panama"),
  list("NTHH", "Saudi Arabian-Iraqi Neutral Zone", NA, NA, "NTHH", D("1922-12-02"), D("1991-12-26"), "historical", "divided between SA and IQ"),
  list("CTKI", "Canton and Enderbury Islands", NA, NA, "CTKI", D("1939-04-06"), D("1979-07-12"), "historical", "to Kiribati"),
  list("FQHH", "French Southern and Antarctic Territories (former code)", NA, NA, "FQHH", NA, D("1979-12-31"), "historical", "now AQ and TF"),
  list("CSH665", "Mandatory Palestine", NA, NA, NA, D("1920-04-26"), D("1948-05-14"), "historical", "synthetic key from CShapes 665"),
  list("CSH3", "Territory of Alaska", NA, NA, NA, D("1867-10-18"), D("1959-01-02"), "historical", "US territory; a US state since 1959"),
  list("CSH4", "Territory of Hawaii", NA, NA, NA, D("1898-07-07"), D("1959-08-20"), "historical", "US territory; a US state since 1959"),
  list("CSH21", "Dominion of Newfoundland", NA, NA, NA, D("1907-09-26"), D("1949-03-31"), "historical", "joined Canada"),
  list("CSH730", "Korea (before partition)", NA, NA, NA, NA, D("1945-08-14"), "historical", "Korean Empire / Japanese Korea; successors KP and KR")
), use.names = FALSE)
setnames(hist, names(cur))


# ---- CShapes code -> entity over periods ---------------------------------------
# curated splits and assignments; everything else follows countrycode when it gives a
# current country, else a synthetic entity CSH<gwcode>
curated_periods <- rbindlist(list(
  list(365L, D("1886-01-01"), D("1922-12-29"), "RU", "Russian Empire and Soviet Russia: same continuing state as the Russian Federation (rename rule)"),
  list(365L, D("1922-12-30"), D("1991-12-25"), "SUHH", "USSR"),
  list(365L, D("1991-12-26"), D("2019-12-31"), "RU", "Russian Federation"),
  list(345L, D("1918-12-01"), D("1992-04-26"), "YUCS", "Yugoslavia to the break-up"),
  list(345L, D("1992-04-27"), D("2006-06-02"), "CSXX", "FR Yugoslavia / Serbia and Montenegro"),
  list(265L, D("1945-05-08"), D("1990-10-02"), "DDDE", "CShapes starts the unit at the Soviet occupation zone (1945); the GDR was founded 1949-10-07"),
  list(315L, D("1918-11-11"), D("1992-12-31"), "CSHH", "Czechoslovakia"),
  list(680L, D("1967-11-30"), D("1990-05-21"), "YDYE", "South Yemen"),
  list(678L, D("1918-10-30"), D("2019-12-31"), "YE", "Yemen Arab Republic continues as Yemen after unification (ISO YE retained)"),
  list(816L, D("1954-05-01"), D("2019-12-31"), "VN", "Democratic Republic of Vietnam continues as Vietnam (rename rule)"),
  list(817L, D("1954-05-01"), D("1975-04-30"), "VNRV", "Republic of Vietnam"),
  list(515L, D("1920-06-28"), D("1962-06-30"), "RUBI", "Ruanda-Urundi"),
  list(255L, D("1886-01-01"), D("1945-05-07"), "DE", "German Empire / Reich: same continuing state as Germany (rename rule)"),
  list(6L, D("1886-01-01"), D("2019-12-31"), "PR", "territory still exists"),
  list(65L, D("1886-01-01"), D("2019-12-31"), "GP", "territory still exists"),
  list(66L, D("1886-01-01"), D("2019-12-31"), "MQ", "territory still exists"),
  list(120L, D("1886-01-01"), D("2019-12-31"), "GF", "territory still exists"),
  list(585L, D("1886-01-01"), D("2019-12-31"), "RE", "territory still exists"),
  list(930L, D("1886-01-01"), D("2019-12-31"), "NC", "territory still exists"),
  list(960L, D("1903-05-19"), D("2019-12-31"), "PF", "territory still exists"),
  list(347L, D("2008-02-20"), D("2019-12-31"), "XK", "Kosovo"),
  list(609L, D("1958-10-10"), D("1975-11-13"), "EH", "Spanish Sahara is today's Western Sahara territory"),
  list(6511L, D("1948-05-14"), D("1967-06-09"), "PS", "Gaza under Egyptian administration; territory of today's PS"),
  list(6631L, D("1948-05-14"), D("1967-06-09"), "PS", "West Bank under Jordanian administration; territory of today's PS"),
  list(665L, D("1920-04-26"), D("1948-05-13"), "CSH665", "Mandatory Palestine"),
  list(3L, D("1886-01-01"), D("1959-01-02"), "CSH3", "Territory of Alaska"),
  list(4L, D("1898-07-06"), D("1959-08-20"), "CSH4", "Territory of Hawaii"),
  list(21L, D("1886-01-01"), D("1948-07-21"), "CSH21", "Newfoundland"),
  list(730L, D("1886-01-01"), D("1945-08-14"), "CSH730", "Korea before partition")
), use.names = FALSE)
setnames(curated_periods, c("gwcode", "from", "to", "entity_key", "note"))
curated_periods[, basis := "curated"]

fwrite(curated_periods, file.path(OUT, "history_curated_periods.csv"))
fwrite(hist, file.path(OUT, "history_curated_entities.csv"))

# ---- lineage ------------------------------------------------------------------
xml <- readLines(file.path(SRC, "cldr_supplementalMetadata.xml"), encoding = "UTF-8", warn = FALSE)
ta <- regmatches(xml, regexec('<territoryAlias type="([^"]+)" replacement="([^"]+)" reason="([^"]+)"', xml))
ta <- rbindlist(lapply(ta[lengths(ta) == 4], function(m) list(code = m[2], replacement = m[3], reason = m[4])))
cldr_succ <- function(cd) strsplit(ta[code == cd, replacement], " ")[[1]]
lin <- rbindlist(list(
  data.table(from_entity = "SUHH", to_entity = cldr_succ("SU"), relation = "dissolution", date = D("1991-12-26"), source = "CLDR territoryAlias SU"),
  data.table(from_entity = "CSHH", to_entity = cldr_succ("200"), relation = "dissolution", date = D("1993-01-01"), source = "CLDR territoryAlias 200"),
  data.table(from_entity = "DDDE", to_entity = cldr_succ("DD"), relation = "absorbed", date = D("1990-10-03"), source = "CLDR territoryAlias DD"),
  data.table(from_entity = "YDYE", to_entity = cldr_succ("YD"), relation = "merged", date = D("1990-05-22"), source = "CLDR territoryAlias YD"),
  data.table(from_entity = "YUCS", to_entity = c("SI", "HR", "MK", "BA", "CSXX"), relation = "dissolution", date = D("1992-04-27"),
             source = "CLDR territoryAlias 890 (RS ME replaced by CSXX, their direct predecessor)"),
  data.table(from_entity = "CSXX", to_entity = cldr_succ("CS"), relation = "dissolution", date = D("2006-06-03"), source = "CLDR territoryAlias CS"),
  data.table(from_entity = "RS", to_entity = "XK", relation = "secession", date = D("2008-02-17"), source = "curated"),
  data.table(from_entity = "ANHH", to_entity = cldr_succ("AN"), relation = "dissolution", date = D("2010-10-10"), source = "CLDR territoryAlias AN"),
  data.table(from_entity = "ANHH", to_entity = "AW", relation = "secession", date = D("1986-01-01"), source = "curated (ISO 3166-3 comment)"),
  data.table(from_entity = "VNRV", to_entity = "VN", relation = "absorbed", date = D("1976-07-02"), source = "curated"),
  data.table(from_entity = "RUBI", to_entity = c("RW", "BI"), relation = "dissolution", date = D("1962-07-01"), source = "curated"),
  data.table(from_entity = "GEHH", to_entity = c("KI", "TV"), relation = "dissolution", date = D("1976-01-01"), source = "ISO 3166-3 comment"),
  data.table(from_entity = "PCHH", to_entity = c("FM", "MH", "MP", "PW"), relation = "dissolution", date = D("1994-10-01"), source = "ISO 3166-3 comment"),
  data.table(from_entity = "SKIN", to_entity = "IN", relation = "absorbed", date = D("1975-05-16"), source = "curated"),
  data.table(from_entity = "PZPA", to_entity = "PA", relation = "absorbed", date = D("1979-10-01"), source = "curated"),
  data.table(from_entity = "NTHH", to_entity = cldr_succ("NT"), relation = "dissolution", date = D("1991-12-26"), source = "CLDR territoryAlias NT"),
  data.table(from_entity = "CTKI", to_entity = "KI", relation = "absorbed", date = D("1979-07-12"), source = "curated"),
  data.table(from_entity = "CSH665", to_entity = c("IL", "PS"), relation = "partition", date = D("1948-05-14"), source = "curated"),
  data.table(from_entity = "CSH3", to_entity = "US", relation = "absorbed", date = D("1959-01-03"), source = "curated"),
  data.table(from_entity = "CSH4", to_entity = "US", relation = "absorbed", date = D("1959-08-21"), source = "curated"),
  data.table(from_entity = "CSH21", to_entity = "CA", relation = "absorbed", date = D("1949-03-31"), source = "curated"),
  data.table(from_entity = "CSH730", to_entity = c("KP", "KR"), relation = "partition", date = D("1945-08-15"), source = "curated"),
  data.table(from_entity = "FQHH", to_entity = cldr_succ("FQ"), relation = "dissolution", date = D("1980-01-01"), source = "CLDR territoryAlias FQ"),
  data.table(from_entity = "CSH291", to_entity = c("DE", "PL"), relation = "territory_to", date = D("1939-09-01"),
             source = "curated: annexed by Germany 1939, part of Poland from 1945"),
  data.table(from_entity = "CSH462", to_entity = "GH", relation = "absorbed", date = D("1957-03-06"),
             source = "curated: joined the Gold Coast at Ghana's independence (outside the one-year geometry window)")
))
fwrite(lin, file.path(OUT, "history_curated_lineage.csv"))

# ---- former codes -> entity -----------------------------------------------------
iso3 <- as.data.table(fromJSON(file.path(SRC, "iso_3166-3.json"))[["3166-3"]])
rename_to <- c(BYAA = "BY", BUMM = "MM", DYBJ = "BJ", HVBF = "BF", NHVU = "VU", RHZW = "ZW", TPTL = "TL", ZRCD = "CD",
               AIDJ = "DJ", FXFR = "FR", VDVN = "VN", BQAQ = "AQ", NQAQ = "AQ", JTUM = "UM", MIUM = "UM", WKUM = "UM", PUUM = "UM")
codes <- iso3[, .(code = alpha_4, code_type = "ISO 3166-3 alpha-4", name, withdrawn = withdrawal_date,
                  entity_key = fifelse(alpha_4 %in% names(rename_to), rename_to[alpha_4], alpha_4))]
codes <- rbind(codes, iso3[, .(code = alpha_2, code_type = "ISO 3166-1 alpha-2 (former)", name, withdrawn = withdrawal_date,
                               entity_key = fifelse(alpha_4 %in% names(rename_to), rename_to[alpha_4], alpha_4))])
fwrite(codes, file.path(OUT, "history_codes.csv"))   # filtered to known entities at build time

# ---- names ------------------------------------------------------------------------
# Names by which records refer to an entity, each with its source. Historical entities
# get all four sources; current entities get only FORMER names (renames of a continuing
# state), since their current names are already in the GNRS reference.
curated_names <- rbindlist(list(
  data.table(entity_key = "SUHH", name = c("USSR", "U.S.S.R.", "U.S.S.R", "Soviet Union", "SSSR", "CCCP", "UdSSR", "URSS",
    "Union of Soviet Socialist Republics", "Former USSR", "Former U.S.S.R.", "Ex-USSR", "Ex USSR", "Former Soviet Union", "USSR (former)")),
  data.table(entity_key = "YUCS", name = c("Yugoslavia", "Jugoslavija", "Jugoslawien", "Yougoslavie", "Yugoslavia (Former)",
    "Former Yugoslavia", "Ex-Yugoslavia", "SFR Yugoslavia", "SFRY", "Socialist Federal Republic of Yugoslavia",
    "Kingdom of Yugoslavia", "Kingdom of Serbs, Croats and Slovenes")),
  data.table(entity_key = "CSXX", name = c("Serbia and Montenegro", "Serbia & Montenegro", "Federal Republic of Yugoslavia",
    "FR Yugoslavia", "Srbija i Crna Gora")),
  data.table(entity_key = "CSHH", name = c("Czechoslovakia", "Czecho-Slovakia", "Tschechoslowakei", "Tchécoslovaquie",
    "Checoslovaquia", "ČSSR", "CSSR", "ČSR", "Czechoslovakia (Former)", "Former Czechoslovakia")),
  data.table(entity_key = "DDDE", name = c("German Democratic Republic", "GDR", "DDR", "East Germany", "Germany, East",
    "Deutsche Demokratische Republik", "RDA")),
  data.table(entity_key = "YDYE", name = c("South Yemen", "People's Democratic Republic of Yemen", "Yemen, Democratic",
    "Democratic Yemen", "PDR Yemen")),
  data.table(entity_key = "VNRV", name = c("Republic of Vietnam", "South Vietnam", "Vietnam, South", "South Viet Nam")),
  data.table(entity_key = "ANHH", name = c("Netherlands Antilles", "Nederlandse Antillen", "Antilles néerlandaises")),
  data.table(entity_key = "RUBI", name = c("Ruanda-Urundi", "Ruanda Urundi")),
  data.table(entity_key = "CSH665", name = c("Mandatory Palestine", "British Mandate of Palestine", "Palestine Mandate")),
  data.table(entity_key = "CSH21", name = c("Newfoundland", "Dominion of Newfoundland")),
  # former names of continuing states (rename rule): attach to the current entity
  data.table(entity_key = "MM", name = c("Burma")),
  data.table(entity_key = "CD", name = c("Zaire", "Zaïre", "Belgian Congo", "Congo (Kinshasa)", "Congo-Kinshasa")),
  data.table(entity_key = "CG", name = c("Congo (Brazzaville)", "Congo-Brazzaville", "French Congo")),
  data.table(entity_key = "BJ", name = c("Dahomey")),
  data.table(entity_key = "BF", name = c("Upper Volta", "Haute-Volta")),
  data.table(entity_key = "LK", name = c("Ceylon")),
  data.table(entity_key = "TH", name = c("Siam")),
  data.table(entity_key = "IR", name = c("Persia")),
  data.table(entity_key = "BY", name = c("Byelorussia", "Belorussia", "Byelorussian SSR", "Belorussian SSR")),
  data.table(entity_key = "ZW", name = c("Rhodesia", "Southern Rhodesia")),
  data.table(entity_key = "ZM", name = c("Northern Rhodesia")),
  data.table(entity_key = "MW", name = c("Nyasaland")),
  data.table(entity_key = "LS", name = c("Basutoland")),
  data.table(entity_key = "BW", name = c("Bechuanaland")),
  data.table(entity_key = "GH", name = c("Gold Coast")),
  data.table(entity_key = "ML", name = c("French Sudan")),
  data.table(entity_key = "BZ", name = c("British Honduras")),
  data.table(entity_key = "GY", name = c("British Guiana")),
  data.table(entity_key = "SR", name = c("Dutch Guiana", "Netherlands Guiana")),
  data.table(entity_key = "GQ", name = c("Spanish Guinea")),
  data.table(entity_key = "ET", name = c("Abyssinia")),
  data.table(entity_key = "KH", name = c("Kampuchea", "Khmer Republic")),
  data.table(entity_key = "VU", name = c("New Hebrides")),
  data.table(entity_key = "TL", name = c("East Timor", "Portuguese Timor")),
  data.table(entity_key = "DJ", name = c("French Somaliland", "French Territory of the Afars and the Issas")),
  data.table(entity_key = "TZ", name = c("Tanganyika")),
  data.table(entity_key = "KI", name = c("Gilbert Islands")),
  data.table(entity_key = "TV", name = c("Ellice Islands"))
))
curated_names[, source := "curated"]
fwrite(curated_names, file.path(OUT, "history_curated_names.csv"))

gn <- as.data.table(arrow::read_parquet(file.path(SRC, "geonames_political_features.parquet")))
# only PCLH records of entities that can carry a GeoNames id: the curated historical
# entities and the current countries (synthetic CShapes entities never have one)
gn <- gn[feature_code == "PCLH" & geonameid %in% c(cur$geonameid, hist$geonameid, "7500737")]
gn[geonameid == "7500737", geonameid := "8505033"]           # FR Yugoslavia -> CSXX entity
gn_names <- gn[, .(name = unique(c(name, asciiname, trimws(strsplit(alternatenames, ",", fixed = TRUE)[[1]])))), by = geonameid]
fwrite(gn_names[nzchar(name)], file.path(OUT, "history_geonames_names.csv"))   # joined to entities at build time

# The rest - CShapes periods for uncurated codes, synthetic entities, coverage,
# geometry-derived lineage, CShapes names and final assembly - runs at build time in
# gnrs_history_assemble(). Remove the tables the previous version of this script wrote,
# which were CShapes-derived and must not ship.
unlink(file.path(OUT, c("history_entities.csv", "cshapes_entity_periods.csv",
                        "history_lineage.csv", "history_names.csv")))
cat(sprintf("curated: %d entities, %d periods, %d lineage links, %d names | GeoNames names: %d | codes: %d
",
            nrow(hist), nrow(curated_periods), nrow(lin), nrow(curated_names), nrow(gn_names[nzchar(name)]), nrow(codes)))
