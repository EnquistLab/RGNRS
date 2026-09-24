# ===========================================================================
# Alternative and superseded sub-national divisions
#
# A large part of the record stream declares a state or county that is not a GADM
# division: Swedish landskap and lappmarker, British Watsonian vice-counties,
# Norwegian counties from after the 2018 and 2020 reforms. Matching those names
# onto the nearest-looking GADM unit is worse than not matching them at all,
# because the coordinate then falls in a different unit and a correctly
# georeferenced, correctly labelled record looks geo-invalid.
#
# This component recognises such names as units in their own right. A unit with a
# known EXTENT - the set of GADM units it covers - can be validated properly: the
# coordinate must fall inside one of them. A unit whose extent has not been sourced
# yet is UNVERIFIABLE: recognised, and never called invalid.
#
# Curation: data-raw/altdiv.R -> inst/extdata/altdiv_{units,names,extent}.csv.
# ===========================================================================

#' @keywords internal
#' @noRd
gnrs_altdiv_path <- function(table, dir = gnrs_cache_dir()) {
  file.path(dir, paste0("altdiv-", table, ".gz.parquet"))
}

#' Build the alternative-division component
#'
#' Internal.  Reads the shipped curation and writes it to the cache.  No
#' download.  When the GADM layer is present the extents are checked against it,
#' so a unit can never claim a GADM division that does not exist.
#' @keywords internal
#' @noRd
gnrs_build_altdiv <- function(dir = gnrs_cache_dir(create = TRUE), quiet = FALSE) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rd <- function(f) utils::read.csv(gnrs_extdata(f), stringsAsFactors = FALSE,
                                    encoding = "UTF-8", na.strings = "")
  units <- rd("altdiv_units.csv")
  names_ <- rd("altdiv_names.csv")
  extent <- rd("altdiv_extent.csv")
  units$valid_from <- as.Date(units$valid_from)
  units$valid_to <- as.Date(units$valid_to)
  if (anyDuplicated(units$entity_key)) stop("Alternative-division keys are not unique.", call. = FALSE)
  stopifnot(all(names_$entity_key %in% units$entity_key),
            all(extent$entity_key %in% units$entity_key))

  known <- unique(extent$entity_key)
  units$extent_known <- units$entity_key %in% known
  if (nrow(extent) && gnrs_is_built("gadm", dir) && file.exists(gnrs_gadm_path(dir))) {
    g <- as.data.frame(nanoparquet::read_parquet(gnrs_gadm_path(dir)))
    have <- unique(c(g$gid_1, g$gid_2))
    have <- have[!is.na(have)]
    # only where the layer covers that country at all: a layer may be partial (and in
    # the tests is synthetic), and an extent naming a country it does not carry is not
    # an error. Within a country the layer does carry, an unknown unit is a typo.
    country <- function(x) sub("[.].*", "", x)
    covered <- unique(country(have))
    check <- extent$gid[country(extent$gid) %in% covered]
    bad <- setdiff(check, have)
    if (length(bad)) {
      stop("Alternative-division extents name GADM units that are not in the layer: ",
           paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
    }
  }

  w <- function(x, table) nanoparquet::write_parquet(x, gnrs_altdiv_path(table, dir), compression = "gzip")
  w(units, "units"); w(names_, "names"); w(extent, "extent")
  provenance <- list(
    source = "altdiv", full_name = "Alternative and superseded sub-national divisions",
    version = "1", downloaded = as.character(Sys.Date()),
    n_units = nrow(units), n_with_extent = length(known), n_names = nrow(names_),
    systems = paste(sort(unique(units$system)), collapse = ", "),
    sources = "Curated for GNRS; Norwegian county reforms of 2018 and 2020; Swedish landskap, lappmarker and lan name forms; Watsonian vice-counties"
  )
  saveRDS(provenance, gnrs_provenance_path("altdiv", dir))
  gnrs_env$altdiv <- NULL
  if (!quiet) {
    message("  ", nrow(units), " alternative or superseded divisions (", length(known),
            " with a known extent), ", nrow(names_), " names")
  }
  invisible(provenance)
}

#' The alternative-division tables, cached for the session
#' @keywords internal
#' @noRd
gnrs_altdiv <- function(dir = gnrs_cache_dir()) {
  paths <- gnrs_altdiv_path(c("units", "names", "extent"), dir)
  if (!all(file.exists(paths))) return(NULL)
  # the cache is for this directory and these files: a call with another dir,
  # or after a rebuild, reads afresh
  stamp <- paste(normalizePath(dir, winslash = "/"), paste(file.mtime(paths), collapse = "|"))
  if (!is.null(gnrs_env$altdiv) && identical(gnrs_env$altdiv_stamp, stamp)) return(gnrs_env$altdiv)
  rd <- function(p) as.data.frame(nanoparquet::read_parquet(p))
  a <- list(units = rd(paths[1]), names = rd(paths[2]), extent = rd(paths[3]))
  a$units$valid_from <- as.Date(a$units$valid_from)
  a$units$valid_to <- as.Date(a$units$valid_to)
  a$names$lower <- gnrs_lower(a$names$name)
  a$country_of <- a$units$country_iso[match(a$names$entity_key, a$units$entity_key)]
  a$exact <- a$names$match == "exact"
  # A name can belong to two units of different systems: "Skane" is a lan and a
  # landskap, "Norrbotten" likewise. Prefer the one whose extent is known, so a
  # coordinate can still be checked; the order here is what match() picks up.
  ord <- order(!a$units$extent_known[match(a$names$entity_key, a$units$entity_key)])
  a$names <- a$names[ord, , drop = FALSE]
  a$country_of <- a$country_of[ord]
  a$exact <- a$names$match == "exact"
  a$key_exact <- paste(a$country_of[a$exact], a$names$lower[a$exact], sep = "\u001f")
  gnrs_env$altdiv <- a
  gnrs_env$altdiv_stamp <- stamp
  a
}

#' Match declared division names against the alternative divisions
#'
#' Internal.  Vectorised.  \code{country_iso} and \code{name} are the declared
#' country and the declared state or county; \code{dates} are the record dates,
#' used only for units that existed over a period.
#'
#' @return data.frame with one row per input: \code{entity_key}, \code{system},
#'   \code{kind}, \code{extent_known}, and \code{in_period} - NA when the unit has
#'   no dates or the record has none, so that a caller can tell "outside its
#'   period" from "no date to check".
#' @keywords internal
#' @noRd
gnrs_altdiv_match <- function(country_iso, name, dates = NULL, dir = gnrs_cache_dir(),
                              altdiv = NULL) {
  n <- length(name)
  out <- data.frame(entity_key = rep(NA_character_, n), system = NA_character_,
                    kind = NA_character_, extent_known = NA, in_period = NA,
                    stringsAsFactors = FALSE)
  # `altdiv` lets a caller that already holds the tables (the resolver, through the
  # backbone) skip the cache lookup
  a <- if (!is.null(altdiv)) altdiv else gnrs_altdiv(dir)
  if (is.null(a) || !n) return(out)
  nm <- gnrs_lower(trimws(ifelse(is.na(name), "", name)))
  cc <- toupper(ifelse(is.na(country_iso), "", country_iso))
  hit <- match(paste(cc, nm, sep = "\u001f"), a$key_exact)
  key <- ifelse(is.na(hit), NA_character_, a$names$entity_key[a$exact][hit])

  # the regex systems (vice-counties are written "VC57 Derbyshire"), tried only where
  # no exact name matched, and only against patterns registered for that country
  rx <- which(!a$exact)
  todo <- which(is.na(key) & nzchar(nm))
  for (r in rx) {
    if (!length(todo)) break
    cand <- todo[cc[todo] == a$country_of[r]]
    if (!length(cand)) next
    ok <- grepl(a$names$name[r], name[cand], perl = TRUE)
    key[cand[ok]] <- a$names$entity_key[r]
    todo <- setdiff(todo, cand[ok])
  }

  i <- match(key, a$units$entity_key)
  out$entity_key <- key
  out$system <- a$units$system[i]
  out$kind <- a$units$kind[i]
  out$extent_known <- a$units$extent_known[i]
  if (!is.null(dates)) {
    from <- a$units$valid_from[i]
    to <- a$units$valid_to[i]
    d <- as.Date(dates)
    # NA where there is nothing to check: no dates on the unit, or none on the record
    out$in_period <- ifelse(is.na(d) | (is.na(from) & is.na(to)), NA,
                            (is.na(from) | d >= from) & (is.na(to) | d <= to))
  }
  out
}

#' The GADM units an alternative division covers
#'
#' Internal.  Returns a character vector of GADM ids, empty when the extent of
#' that unit has not been sourced.
#' @keywords internal
#' @noRd
gnrs_altdiv_extent <- function(entity_key, dir = gnrs_cache_dir()) {
  a <- gnrs_altdiv(dir)
  if (is.null(a) || is.na(entity_key)) return(character(0))
  a$extent$gid[a$extent$entity_key == entity_key]
}
