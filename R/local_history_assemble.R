# ===========================================================================
# Assembling the history tables at build time
#
# CShapes 2.0 is CC BY-NC-SA 4.0 and this package is MIT, so nothing derived from
# CShapes ships with it (BM, 2026-09-22). The package carries only the curation
# (inst/extdata, from data-raw/history_crosswalk.R) and extracts from
# redistributable sources. Everything that depends on CShapes - the periods of
# every state code the curation does not assign, the synthetic CSH<gwcode>
# entities and their names, the coverage check, and the lineage derived from
# CShapes geometry - is computed here from the user's own copy of CShapes, which
# GNRS_local_build("cshapes") puts in the cache.
#
# This reproduces what data-raw/history_crosswalk.R computed before 2026-09-22
# step for step; the test suite checks the result against those tables.
# ===========================================================================

#' Minimum share of a predecessor's area a successor must receive to be linked
#' @keywords internal
#' @noRd
gnrs_history_min_share <- function() 0.05

#' Days after a predecessor ends within which a successor may first appear
#'
#' Successor states often appear weeks after the predecessor ends
#' (Austria-Hungary ended 1918-11-02; Czechoslovakia starts 1918-11-11).
#' @keywords internal
#' @noRd
gnrs_history_window_days <- function() 365

#' Names kept out of fuzzy matching
#'
#' Short former names that are prefixes or near-duplicates of other entities'
#' names ("Rhodesia": "N. Rhodesia", which is Zambia, fuzzy-matched it).
#' @keywords internal
#' @noRd
gnrs_history_exact_only_names <- function() c("Rhodesia")

#' Assemble the history tables from the curation and the cached CShapes
#'
#' Internal.  Returns a list of data frames - \code{ent}, \code{nm}, \code{lin},
#' \code{per}, \code{codes} - with the columns and types of the tables that
#' previously shipped in inst/extdata, so \code{gnrs_build_history()} treats them
#' exactly as it treated those.
#' @keywords internal
#' @noRd
gnrs_history_assemble <- function(dir = gnrs_cache_dir(), quiet = FALSE) {
  for (pkg in c("sf", "countrycode")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Building the history component needs the '", pkg, "' package.", call. = FALSE)
    }
  }
  if (!file.exists(gnrs_cshapes_versions_path(dir)) || !file.exists(gnrs_cshapes_geom_path(dir))) {
    stop("The history component is derived from CShapes, which has not been built. ",
         "Run GNRS_local_build(\"cshapes\").", call. = FALSE)
  }
  # Only empty cells are missing: "NA" is Namibia's ISO code
  rd <- function(f) utils::read.csv(gnrs_extdata(f), stringsAsFactors = FALSE, encoding = "UTF-8",
                                    na.strings = "", colClasses = "character")
  num <- function(x) as.integer(x)
  day <- function(x) as.Date(x)

  # ---- current entities: the snapshot shipped with the curation ---------------
  # Fixed with the curation, as it always was: which uncurated state codes map to a
  # current country depends on this list, so it must not drift with the user's backbone.
  cur <- rd("history_current_entities.csv")
  cur$valid_from <- day(cur$valid_from)
  cur$valid_to <- day(cur$valid_to)

  # ---- curated historical entities -------------------------------------------
  hist <- rd("history_curated_entities.csv")
  hist$valid_from <- day(hist$valid_from)
  hist$valid_to <- day(hist$valid_to)

  # ---- CShapes, from the cache -------------------------------------------------
  csd <- as.data.frame(nanoparquet::read_parquet(gnrs_cshapes_versions_path(dir)))
  csd <- data.frame(version_id = csd$version_id, gwcode = as.integer(csd$gwcode),
                    country_name = csd$country_name, start = day(csd$valid_from),
                    end = day(csd$valid_to), stringsAsFactors = FALSE)
  by_gw <- split(seq_len(nrow(csd)), csd$gwcode)
  g <- data.frame(
    gwcode = as.integer(names(by_gw)),
    first = as.Date(vapply(by_gw, function(i) as.numeric(min(csd$start[i])), numeric(1)), origin = "1970-01-01"),
    last = as.Date(vapply(by_gw, function(i) as.numeric(max(csd$end[i])), numeric(1)), origin = "1970-01-01"),
    cs_name = vapply(by_gw, function(i) csd$country_name[i][which.max(csd$end[i])], character(1)),
    stringsAsFactors = FALSE
  )
  g$iso2 <- suppressWarnings(countrycode::countrycode(g$gwcode, "gwn", "iso2c", warn = FALSE))

  # ---- state code -> entity over periods ---------------------------------------
  # curated assignments; every other code follows countrycode when it gives a current
  # country, else becomes a synthetic entity CSH<gwcode>
  cp <- rd("history_curated_periods.csv")
  curated_periods <- data.frame(gwcode = num(cp$gwcode), from = day(cp$from), to = day(cp$to),
                                entity_key = cp$entity_key, note = cp$note, basis = cp$basis,
                                stringsAsFactors = FALSE)
  auto <- g[!g$gwcode %in% curated_periods$gwcode, , drop = FALSE]
  auto$entity_key <- ifelse(!is.na(auto$iso2) & auto$iso2 %in% cur$entity_key, auto$iso2,
                            paste0("CSH", auto$gwcode))
  syn <- startsWith(auto$entity_key, "CSH")
  auto_periods <- data.frame(gwcode = auto$gwcode, from = auto$first, to = auto$last,
                             entity_key = auto$entity_key,
                             note = ifelse(syn, paste("synthetic entity:", auto$cs_name), NA_character_),
                             basis = ifelse(syn, "synthetic", "countrycode gwn->iso2c"),
                             stringsAsFactors = FALSE)
  periods <- rbind(curated_periods, auto_periods)
  periods <- periods[order(periods$gwcode, periods$from), , drop = FALSE]

  # every other CShapes state without a current country is a synthetic historical
  # entity, named as CShapes names it (colonies, protectorates, former states)
  synth <- data.frame(entity_key = auto$entity_key[syn], name = auto$cs_name[syn],
                      geonameid = NA_character_, iso3166_1 = NA_character_, iso3166_3 = NA_character_,
                      valid_from = auto$first[syn], valid_to = auto$last[syn], kind = "historical",
                      note = "synthetic entity from a CShapes state with no current country",
                      stringsAsFactors = FALSE)
  entities <- rbind(cur, hist, synth)
  if (anyDuplicated(entities$entity_key)) stop("History entity keys are not unique.", call. = FALSE)
  missing_ent <- setdiff(periods$entity_key, entities$entity_key)
  if (length(missing_ent)) {
    stop("History periods reference unknown entities: ", paste(missing_ent, collapse = ", "), call. = FALSE)
  }

  # every CShapes version must be covered by an entity period on the days it is valid
  covered <- vapply(seq_len(nrow(csd)), function(i) {
    any(periods$gwcode == csd$gwcode[i] & periods$from <= csd$end[i] & periods$to >= csd$start[i])
  }, logical(1))
  if (!all(covered)) {
    stop("CShapes versions not covered by any entity period: ",
         paste(csd$version_id[!covered], collapse = ", "), call. = FALSE)
  }

  # ---- lineage ---------------------------------------------------------------------
  cl <- rd("history_curated_lineage.csv")
  lin <- data.frame(from_entity = cl$from_entity, to_entity = cl$to_entity, relation = cl$relation,
                    date = day(cl$date), source = cl$source, stringsAsFactors = FALSE)
  geo_lin <- gnrs_history_geometry_lineage(dir, csd, periods, entities, lin, quiet = quiet)
  lin <- rbind(lin, geo_lin)
  bad <- setdiff(c(lin$from_entity, lin$to_entity), entities$entity_key)
  if (length(bad)) stop("History lineage references unknown entities: ", paste(bad, collapse = ", "), call. = FALSE)

  # ---- former codes -> entity -------------------------------------------------------
  codes <- rd("history_codes.csv")
  codes <- codes[codes$entity_key %in% entities$entity_key, , drop = FALSE]

  # ---- names -------------------------------------------------------------------------
  # Names by which records refer to an entity, each with its source. The order of the
  # sources matters: where two give the same name, the first keeps it.
  hist_keys <- entities$entity_key[entities$kind == "historical"]
  ent_names <- data.frame(entity_key = hist_keys, name = entities$name[entities$kind == "historical"],
                          source = "entity name", stringsAsFactors = FALSE)
  curated_names <- rd("history_curated_names.csv")[, c("entity_key", "name", "source")]
  cs_names <- merge(periods[, c("gwcode", "entity_key")],
                    data.frame(gwcode = csd$gwcode, name = csd$country_name, stringsAsFactors = FALSE),
                    by = "gwcode")
  cs_names <- cs_names[cs_names$entity_key %in% hist_keys, c("entity_key", "name")]
  cs_names$source <- "CShapes 2.0"
  cs_names <- unique(cs_names)
  gn <- rd("history_geonames_names.csv")
  gn_names <- merge(gn, entities[!is.na(entities$geonameid), c("geonameid", "entity_key")], by = "geonameid")
  gn_names <- gn_names[nzchar(gn_names$name), c("entity_key", "name")]
  gn_names$source <- "GeoNames (CC BY 4.0)"
  iso_names <- unique(data.frame(entity_key = codes$entity_key, name = codes$name, source = "ISO 3166-3",
                                 stringsAsFactors = FALSE))
  hnames <- rbind(ent_names, curated_names, cs_names, gn_names, iso_names)
  hnames <- hnames[!duplicated(hnames[, c("entity_key", "name")]), , drop = FALSE]
  hnames <- hnames[nzchar(trimws(hnames$name)), , drop = FALSE]
  # fuzzy: may this name take part in GNRS fuzzy matching? Exact-only for names of
  # synthetic entities made from CShapes colonies (obscure names that drew fuzzy matches
  # from current countries on GBIF data: "Northern Rhodesia> Zambia" -> "Northeastern
  # Rhodesia") and for short former names that are near-duplicates of other entities'
  hnames$fuzzy <- !grepl("^CSH[0-9]+$", hnames$entity_key) &
    !(hnames$name %in% gnrs_history_exact_only_names())
  bad <- setdiff(hnames$entity_key, entities$entity_key)
  if (length(bad)) stop("History names reference unknown entities: ", paste(bad, collapse = ", "), call. = FALSE)

  if (!quiet) {
    message("  from CShapes: ", nrow(synth), " synthetic entities, ", sum(periods$basis != "curated"),
            " uncurated state-code periods, ", nrow(geo_lin), " geometry-derived lineage links")
  }
  lapply(list(ent = entities, nm = hnames, lin = lin, per = periods, codes = codes), gnrs_csv_types)
}

#' Successors of historical entities without curated lineage, from CShapes geometry
#'
#' Internal.  Mostly CShapes colonies and protectorates: the entity's last polygon
#' is intersected with the CShapes versions valid on the day after it ends, or, for
#' states with no such version, their first version starting within
#' \code{gnrs_history_window_days()}; every successor receiving at least
#' \code{gnrs_history_min_share()} of its area is linked. Shares are planar in
#' degrees, adequate for apportioning one territory among neighbours. Geometry is
#' used here only.
#' @keywords internal
#' @noRd
gnrs_history_geometry_lineage <- function(dir, csd, periods, entities, lin, quiet = FALSE) {
  min_share <- gnrs_history_min_share()
  window <- gnrs_history_window_days()
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  cs <- sf::st_read(gnrs_cshapes_geom_path(dir), layer = "country", quiet = TRUE)
  m <- match(cs$version_id, csd$version_id)
  if (anyNA(m)) stop("CShapes geometry and versions disagree; rebuild the CShapes component.", call. = FALSE)
  cs$gwcode <- csd$gwcode[m]
  cs$start <- csd$start[m]
  cs$end <- csd$end[m]

  ent_of <- function(gw, d) {
    hit <- periods$entity_key[periods$gwcode == gw & periods$from <= d & periods$to >= d]
    if (length(hit)) hit[1] else NA_character_
  }
  need <- setdiff(entities$entity_key[entities$kind == "historical"], lin$from_entity)
  out <- list()
  for (ek in need) {
    pr <- periods[periods$entity_key == ek, , drop = FALSE]
    if (!nrow(pr)) next
    last_day <- max(pr$to)
    if (last_day >= as.Date("2019-12-31")) next
    last_v <- cs[cs$gwcode %in% pr$gwcode & cs$start <= last_day & cs$end >= last_day, ]
    if (!nrow(last_v)) next
    # planar on purpose (see above); sf says so on every call, so it is silenced
    pred <- suppressMessages(sf::st_union(sf::st_make_valid(sf::st_geometry(last_v))))
    a0 <- as.numeric(sf::st_area(sf::st_set_crs(pred, NA)))
    on_day <- cs$start <= last_day + 1 & cs$end >= last_day + 1
    soon <- cs$start > last_day + 1 & cs$start <= last_day + window & !(cs$gwcode %in% cs$gwcode[on_day])
    nxt <- cs[on_day | soon, ]
    nxt <- nxt[order(nxt$gwcode, nxt$start), ]
    nxt <- nxt[!duplicated(nxt$gwcode), ]
    nxt <- nxt[lengths(suppressMessages(sf::st_intersects(nxt, pred))) > 0, ]
    if (!nrow(nxt)) next
    inter <- suppressMessages(suppressWarnings(sf::st_intersection(sf::st_make_valid(sf::st_geometry(nxt)), pred)))
    share <- as.numeric(sf::st_area(sf::st_set_crs(inter, NA))) / a0
    keep <- share >= min_share
    if (!any(keep)) next
    gw <- nxt$gwcode[keep]
    d <- pmax(last_day + 1, nxt$start[keep])
    to <- vapply(seq_along(gw), function(i) ent_of(gw[i], d[i]), character(1))
    ok <- !is.na(to) & to != ek
    if (!any(ok)) next
    s <- tapply(share[keep][ok], factor(to[ok], levels = unique(to[ok])), sum)
    out[[ek]] <- data.frame(from_entity = ek, to_entity = names(s), relation = "territory_to",
                            date = last_day + 1, source = sprintf("CShapes geometry overlap (share %.2f)", as.numeric(s)),
                            stringsAsFactors = FALSE)
  }
  if (!length(out)) {
    return(data.frame(from_entity = character(0), to_entity = character(0), relation = character(0),
                      date = as.Date(character(0)), source = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, unname(out))
}

#' Give a table the column types it would have if read back from CSV
#'
#' Internal.  gnrs_build_history() was written against tables read from shipped
#' CSVs (dates and codes as text, identifiers as integers); a round trip through a
#' temporary CSV reproduces those types exactly, so the cache is unchanged by the
#' move from shipped tables to tables assembled at build time.
#' @keywords internal
#' @noRd
gnrs_csv_types <- function(x) {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  for (j in seq_along(x)) if (inherits(x[[j]], "Date")) x[[j]] <- format(x[[j]])
  utils::write.csv(x, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "")
}
