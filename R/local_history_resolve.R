# ===========================================================================
# The history component: entities, names, lineage, CShapes periods and former
# codes, assembled at build time from the curation shipped in inst/extdata
# (data-raw/history_crosswalk.R) and the user's cached CShapes
# (gnrs_history_assemble(), R/local_history_assemble.R), and resolution against them.
# ===========================================================================

#' Path of a shipped history table
#'
#' Internal.  In an installed package these are under inst/extdata; while
#' developing from source, set \code{options(GNRS.extdata_dir = )}.
#' @keywords internal
#' @noRd
gnrs_extdata <- function(file) {
  dev <- getOption("GNRS.extdata_dir")
  if (!is.null(dev)) {
    return(file.path(dev, file))
  }
  path <- system.file("extdata", file, package = "GNRS")
  if (!nzchar(path)) stop("History table not found in the package: ", file, call. = FALSE)
  path
}

#' @keywords internal
#' @noRd
gnrs_history_path <- function(table, dir = gnrs_cache_dir()) {
  file.path(dir, paste0("history-", table, ".gz.parquet"))
}

#' First identifier of the range reserved for entities without a GeoNames id
#'
#' GeoNames identifiers are below 13 million; the reserved range starts well above.
#' @keywords internal
#' @noRd
gnrs_history_synthetic_base <- function() 990000000L

#' Build the history component
#'
#' Internal.  Assembles the tables (gnrs_history_assemble()), gives every entity a numeric identifier
#' (its GeoNames id where it has one, so results join to GeoNames and to the web
#' service; otherwise one in a reserved range, stable across builds because it is
#' assigned in order of entity key), and writes them to the cache.  No download,
#' but it needs the CShapes component built first.
#'
#' Names that collide with a CURRENT country's standard or alternate name are
#' handled so that a current country wins ties: a colliding name from a
#' non-curated source is dropped; a colliding curated name is kept and reported,
#' because it was added deliberately.
#' @keywords internal
#' @noRd
gnrs_build_history <- function(dir = gnrs_cache_dir(create = TRUE), quiet = FALSE) {
  # Nothing derived from CShapes ships (it is CC BY-NC-SA 4.0; the package is MIT):
  # the tables are assembled here from the curation and the user's cached CShapes.
  h <- gnrs_history_assemble(dir = dir, quiet = quiet)
  ent <- h$ent
  nm <- h$nm
  lin <- h$lin
  per <- h$per
  codes <- h$codes

  synth <- sort(ent$entity_key[is.na(ent$geonameid)])
  ent$entity_id <- suppressWarnings(as.integer(ent$geonameid))
  no_id <- is.na(ent$entity_id)
  ent$entity_id[no_id] <- gnrs_history_synthetic_base() + match(ent$entity_key[no_id], synth)
  if (anyDuplicated(ent$entity_id)) stop("History entity identifiers are not unique.", call. = FALSE)
  ent$short_name <- sub(" \\(.*\\)$", "", ent$name)
  id_of <- function(k) ent$entity_id[match(k, ent$entity_key)]

  nm$entity_id <- id_of(nm$entity_key)
  lin$from_id <- id_of(lin$from_entity)
  lin$to_id <- id_of(lin$to_entity)
  per$entity_id <- id_of(per$entity_key)
  codes$entity_id <- id_of(codes$entity_key)

  # collisions with current country names (standard and GeoNames alternate)
  collisions <- data.frame(entity_key = character(0), name = character(0),
                           source = character(0), current_country_id = integer(0))
  if (gnrs_is_built("gnrs", dir)) {
    cur_country <- as.data.frame(nanoparquet::read_parquet(gnrs_reference_path("country", dir)))
    cur_names <- data.frame(id = integer(0), name = character(0))
    if (file.exists(gnrs_names_path(dir))) {
      x <- as.data.frame(nanoparquet::read_parquet(gnrs_names_path(dir)))
      cur_names <- x[x$level == "country", c("id", "name"), drop = FALSE]
    }
    current_lower <- gnrs_lower(c(cur_country$country, cur_names$name))
    current_id <- c(cur_country$country_id, cur_names$id)
    hit <- match(gnrs_lower(nm$name), current_lower)
    clash <- !is.na(hit) & current_id[hit] != nm$entity_id
    if (any(clash)) {
      collisions <- nm[clash, c("entity_key", "name", "source")]
      collisions$current_country_id <- current_id[hit[clash]]
    }
    drop <- clash & nm$source != "curated"
    nm <- nm[!drop, , drop = FALSE]
    if (!quiet && any(clash)) {
      message("  ", sum(drop), " historical names dropped because a current country has the same name; ",
              sum(clash & !drop), " curated collisions kept (see history-collisions)")
    }
  }

  w <- function(x, table) nanoparquet::write_parquet(x, gnrs_history_path(table, dir), compression = "gzip")
  w(ent, "entities")
  w(nm, "names")
  w(lin, "lineage")
  w(per, "periods")
  w(codes, "codes")
  w(collisions, "collisions")

  provenance <- list(
    source = "history", full_name = "Historical political divisions (entities, names, lineage)",
    version = "1", downloaded = as.character(Sys.Date()),
    n_entities = nrow(ent), n_historical = sum(ent$kind == "historical"),
    n_names = nrow(nm), n_lineage = nrow(lin), n_periods = nrow(per),
    sources = paste("Curated; derived at build time from the user's CShapes 2.0 copy (CC BY-NC-SA 4.0,",
                    "not redistributed); GeoNames (CC BY 4.0); Unicode CLDR; ISO 3166-3 via Debian iso-codes")
  )
  saveRDS(provenance, gnrs_provenance_path("history", dir))
  gnrs_env$history <- NULL
  if (!quiet) {
    message("  ", provenance$n_historical, " historical entities, ", provenance$n_names,
            " names, ", provenance$n_lineage, " lineage links")
  }
  invisible(provenance)
}

#' The history tables, cached for the session
#' @keywords internal
#' @noRd
gnrs_history <- function(dir = gnrs_cache_dir()) {
  paths <- gnrs_history_path(c("entities", "names", "lineage", "periods"), dir)
  if (!all(file.exists(paths))) {
    stop("The history component has not been built. Run GNRS_local_build(\"history\").", call. = FALSE)
  }
  key <- paste(paths, file.mtime(paths), collapse = "|")
  if (identical(gnrs_env$history_key, key) && !is.null(gnrs_env$history)) {
    return(gnrs_env$history)
  }
  rd <- function(p) as.data.frame(nanoparquet::read_parquet(p))
  h <- list(entities = rd(paths[1]), names = rd(paths[2]), lineage = rd(paths[3]), periods = rd(paths[4]))
  gnrs_env$history <- h
  gnrs_env$history_key <- key
  h
}

#' The backbone with historical countries added
#'
#' Internal.  Historical entities are appended to the country table and their
#' names (and former names of continuing states) to the country name table, as
#' alternate names, so the unchanged cascade finds them by exact or fuzzy
#' alternate name.  They carry no ISO or GADM codes.  The copy has its own match
#' cache, so indexes built for the current-only backbone are not reused.
#' @keywords internal
#' @noRd
gnrs_backbone_with_history <- function(bb, dir = gnrs_cache_dir()) {
  h <- gnrs_history(dir)
  he <- h$entities[h$entities$kind == "historical", , drop = FALSE]
  co <- bb$country
  add <- co[rep(NA_integer_, nrow(he)), , drop = FALSE]
  add$country_id <- he$entity_id
  add$country <- he$short_name
  if ("is_geoname" %in% names(add)) add$is_geoname <- !is.na(he$geonameid)
  add$lower <- gnrs_lower(add$country)
  # Synthetic entities made from CShapes colonies match their standard name exactly only;
  # per-name fuzzy flags for alternate names come from the shipped names table. On GBIF
  # names, fuzzy matching of such names drew records away from the right current country
  # ("N. Rhodesia", correctly Zambia, went to Zimbabwe via "Rhodesia").
  # synthetic keys are CSH followed by the CShapes code; note Czechoslovakia is CSHH
  synthetic <- grepl("^CSH[0-9]+$", he$entity_key)
  co$fuzzy <- TRUE
  add$fuzzy <- !synthetic
  country <- rbind(co, add)
  country <- country[order(country$country_id), , drop = FALSE]
  rownames(country) <- NULL

  cn <- bb$country_names
  new_names <- data.frame(
    level = "country", id = h$names$entity_id, name = h$names$name,
    name_type = paste("historical:", h$names$source), stringsAsFactors = FALSE
  )
  new_names <- new_names[!is.na(new_names$id), , drop = FALSE]
  new_names$lower <- gnrs_lower(new_names$name)
  new_names$original <- TRUE
  # per-name decision shipped in history_names.csv (data-raw/history_crosswalk.R)
  new_names$fuzzy <- as.logical(h$names$fuzzy[!is.na(h$names$entity_id)])
  cn$fuzzy <- TRUE
  for (col in setdiff(names(cn), names(new_names))) new_names[[col]] <- NA
  new_names <- new_names[, names(cn), drop = FALSE]
  country_names <- rbind(cn, new_names)
  country_names <- country_names[order(country_names$id, country_names$name), , drop = FALSE]
  rownames(country_names) <- NULL

  bb2 <- bb
  bb2$country <- country
  bb2$country_names <- country_names
  bb2$country_name_rows_by_id <- split(seq_len(nrow(country_names)), country_names$id)
  bb2$cache <- new.env(parent = emptyenv())
  bb2$history_ids <- he$entity_id
  bb2
}

#' Parse the optional date column: a year, an ISO date, or a Date
#' @keywords internal
#' @noRd
gnrs_parse_record_date <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  year <- !is.na(x) & grepl("^[0-9]{3,4}$", x)
  if (any(year)) out[year] <- as.Date(sprintf("%04d-07-01", as.integer(x[year])))
  iso <- !is.na(x) & !year & grepl("^[0-9]{4}-[0-9]{2}", x)
  # guarded: paste0(character(0), "-01") is "-01", not character(0)
  if (any(iso)) out[iso] <- suppressWarnings(as.Date(substr(paste0(x[iso], "-01"), 1, 10)))
  out
}

#' Does a submitted name mark the division as former?
#'
#' Internal.  "Former USSR", "Ex-USSR", "EX-YUGOSLAVIA", "Czechoslovakia (former)",
#' "formerly Zaire", and the German, Spanish and French equivalents ("ehemalige",
#' "antigua", "ancienne").
#' @keywords internal
#' @noRd
gnrs_is_former_label <- function(x) {
  x <- gnrs_unaccent(tolower(ifelse(is.na(x), "", x)))
  grepl("(^|[^a-z])(former|formerly|ex|ehemalig[a-z]*|antigu[ao]s?|ancien(ne)?s?)([^a-z]|$)", x, perl = TRUE)
}

#' Current successors of a historical entity, following the lineage to the present
#'
#' Internal.  The lineage is followed through current countries as well as
#' former ones: a current country can itself have later successors (Kosovo
#' seceded from Serbia in 2008, so it descends from Yugoslavia through
#' Serbia), and every node is visited once.
#' @keywords internal
#' @noRd
gnrs_current_successors <- function(entity_id, h) {
  cur <- h$entities$entity_id[h$entities$kind == "current"]
  seen <- entity_id
  front <- entity_id
  out <- integer(0)
  while (length(front)) {
    nxt <- setdiff(unique(h$lineage$to_id[h$lineage$from_id %in% front]), seen)
    seen <- c(seen, nxt)
    out <- c(out, intersect(nxt, cur))
    front <- nxt
  }
  unique(out)
}

#' Resolve with historical divisions
#'
#' Internal.  \code{u} is the distinct-row table GNRS_local passes to
#' \code{gnrs_resolve()}; \code{m} maps input rows to it; \code{dates} are the
#' parsed input dates (or NULL).  Returns the per-input-row resolution plus the
#' history columns.
#'
#' \describe{
#'   \item{history = "all"}{Resolve against current and historical countries
#'     together.  A current country wins an exact tie (colliding historical names
#'     from non-curated sources were dropped at build).}
#'   \item{history = "at_date"}{As "all", but a historical match whose validity
#'     does not include the record's date (with \code{tolerance_years} either
#'     side) is replaced by the current-only resolution of that row.  Rows without
#'     a date keep the "all" answer, flagged.}
#' }
#' A state or county submitted under a historical country is resolved within the
#' entity's current successors (the USSR's successors for "USSR" / "Kamchatka"),
#' and the successor used is reported; if none contains it, the sub-national
#' result is "unverifiable".
#' @keywords internal
#' @noRd
gnrs_resolve_history <- function(u, m, dates, bb_current, dir, threshold, history, tolerance_years = 1) {
  h <- gnrs_history(dir)
  bb_all <- gnrs_backbone_with_history(bb_current, dir)

  old <- gnrs_env$backbone
  on.exit(gnrs_env$backbone <- old, add = TRUE)

  gnrs_env$backbone <- bb_all
  r_all <- gnrs_resolve(u, bb_all, threshold = threshold)
  gnrs_env$backbone <- bb_current
  r_cur <- gnrs_resolve(u, bb_current, threshold = threshold)

  ent <- h$entities
  is_hist_id <- function(id) !is.na(id) & id %in% bb_all$history_ids

  # ---- sub-national names under a historical country: resolve within successors
  r_all$subnational_resolved_in <- NA_character_
  r_all$subnational_status <- NA_character_
  hrows <- which(is_hist_id(r_all$country_id))
  sub_rows <- hrows[!gnrs_blank(u$state_province_verbatim[hrows]) | !gnrs_blank(u$county_parish_verbatim[hrows])]
  if (length(sub_rows)) {
    cand <- do.call(rbind, lapply(sub_rows, function(i) {
      succ <- gnrs_current_successors(r_all$country_id[i], h)
      if (!length(succ)) return(NULL)
      data.frame(row = i, successor_id = succ,
                 country_verbatim = bb_current$country$country[match(succ, bb_current$country$country_id)],
                 state_province_verbatim = u$state_province_verbatim[i],
                 county_parish_verbatim = u$county_parish_verbatim[i], stringsAsFactors = FALSE)
    }))
    if (!is.null(cand) && nrow(cand)) {
      cand <- cand[!is.na(cand$country_verbatim), , drop = FALSE]
      rs <- gnrs_resolve(cand[, c("country_verbatim", "state_province_verbatim", "county_parish_verbatim")],
                         bb_current, threshold = threshold)
      rs$row <- cand$row
      rs$successor_id <- cand$successor_id
      ok <- !is.na(rs$state_province_id)
      rs <- rs[ok, , drop = FALSE]
      if (nrow(rs)) {
        exact <- !grepl("^fuzzy", rs$match_method_state_province)
        score <- ifelse(is.na(rs$match_score_state_province), 1, rs$match_score_state_province)
        rs <- rs[order(rs$row, -exact, -score, rs$successor_id), , drop = FALSE]
        rs <- rs[!duplicated(rs$row), , drop = FALSE]
        cols <- c("state_province", "state_province_id", "county_parish", "county_parish_id",
                  "match_method_state_province", "match_method_county_parish",
                  "match_score_state_province", "match_score_county_parish",
                  "state_province_iso", "county_parish_iso", "gid_1", "gid_2")
        cols <- intersect(cols, intersect(names(rs), names(r_all)))
        r_all[rs$row, cols] <- rs[, cols]
        r_all$subnational_resolved_in[rs$row] <- ent$entity_key[match(rs$successor_id, ent$entity_id)]
        r_all$subnational_status[rs$row] <- "resolved in successor"
        # the match level and status were summarized before the state was filled in
        sv <- u$state_province_verbatim[rs$row]
        cv <- u$county_parish_verbatim[rs$row]
        lvl_sub <- ifelse(gnrs_blank(cv), "state_province", "county_parish")
        lvl_mat <- ifelse(is.na(r_all$county_parish_id[rs$row]), "state_province", "county_parish")
        r_all$poldiv_submitted[rs$row] <- lvl_sub
        r_all$poldiv_matched[rs$row] <- lvl_mat
        r_all$match_status[rs$row] <- ifelse(lvl_sub == lvl_mat, "full match", "partial match")
      }
    }
    unres <- sub_rows[is.na(r_all$subnational_status[sub_rows])]
    r_all$subnational_status[unres] <- "unverifiable"
  }

  # ---- per input row
  out <- r_all[m, , drop = FALSE]
  cur_rows <- r_cur[m, , drop = FALSE]
  hist_match <- is_hist_id(out$country_id)
  e <- match(out$country_id, ent$entity_id)
  out$entity_key <- ent$entity_key[e]
  out$is_historical <- ifelse(is.na(out$country_id), NA, hist_match)
  out$entity_valid_from <- ent$valid_from[e]
  out$entity_valid_to <- ent$valid_to[e]
  out$successors <- vapply(seq_len(nrow(out)), function(i) {
    if (!isTRUE(hist_match[i])) return(NA_character_)
    s <- gnrs_current_successors(out$country_id[i], h)
    paste(ent$entity_key[match(s, ent$entity_id)], collapse = ";")
  }, character(1))
  out$date_check <- NA_character_

  if (history == "at_date") {
    from <- as.Date(out$entity_valid_from)
    to <- as.Date(out$entity_valid_to)
    tol <- 365.25 * tolerance_years
    has_date <- !is.null(dates) & !is.na(dates)
    if (is.null(dates)) has_date <- rep(FALSE, nrow(out))
    inside <- (is.na(from) | dates >= from - tol) & (is.na(to) | dates <= to + tol)
    # A name that marks the division as former ("Former USSR", "Ex-Yugoslavia",
    # "Czechoslovakia (former)") is correct for any date after the division ended: it
    # places the record in the former territory (BM, 2026-09-17). Dates before the
    # division existed are still rejected.
    former_label <- gnrs_is_former_label(out$country_verbatim)
    after_end <- has_date & !is.na(to) & dates > to + tol
    exempt <- hist_match & !inside & former_label & after_end
    # A state or county resolved within a successor is kept, and flagged, even when the
    # record's date falls outside the former country's existence ("USSR" + "Kamchatka"
    # dated 2005): the place is identified, only the country label is out of date
    # (BM, 2026-09-17).
    kept_sub <- hist_match & has_date & !inside & !exempt & (out$subnational_status %in% "resolved in successor")
    replace <- hist_match & has_date & !inside & !exempt & !kept_sub
    out$date_check[hist_match & has_date & inside] <- "historical entity valid at record date"
    out$date_check[exempt] <- "record dated after the entity ended, but named as former; historical match kept"
    out$date_check[kept_sub] <- paste("historical match outside its validity at the record date;",
                                      "kept because the state or county resolves in a successor")
    out$date_check[hist_match & !has_date] <- "no record date; historical match kept"
    if (any(replace)) {
      keep_cols <- names(cur_rows)
      out[replace, keep_cols] <- cur_rows[replace, keep_cols]
      out$entity_key[replace] <- ent$entity_key[match(out$country_id[replace], ent$entity_id)]
      out$is_historical[replace] <- FALSE
      out$entity_valid_from[replace] <- NA
      out$entity_valid_to[replace] <- NA
      out$successors[replace] <- NA
      out$subnational_resolved_in[replace] <- NA
      out$subnational_status[replace] <- NA
      out$date_check[replace] <- "historical match outside its validity at the record date; current-only resolution used"
    }
  }
  out
}
