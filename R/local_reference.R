#' Session cache for the loaded reference data
#' @keywords internal
#' @noRd
gnrs_env <- new.env(parent = emptyenv())

#' Drop the loaded reference data from the session
#'
#' Internal.  Called after a build or removal so that the next call reads what
#' is on disk rather than what was.
#' @keywords internal
#' @noRd
gnrs_forget_backbone <- function() {
  rm(list = ls(gnrs_env, all.names = TRUE), envir = gnrs_env)
  invisible(NULL)
}

#' Countries the reference treats as countries but which are subdivisions
#'
#' Internal.  From \code{gnrs_db/sql/state_as_country.sql}: each is linked to
#' the country it is a subdivision of, so that a submission such as country
#' "United States", state "Puerto Rico" resolves to the country Puerto Rico.
#' Names are matched exactly as spelled in the reference, as upstream does; the
#' upstream file also lists "Cook Islands" and "South Georgia and the South
#' Sandwich Islands", but the first is preceded by a malformed statement and the
#' second carries a stray leading space, and the web service confirms that
#' neither link was applied, so they are left out here too.
#' @return A data.frame of child and parent country names.
#' @keywords internal
#' @noRd
gnrs_state_as_country_links <- function() {
  child <- c(
    "Akrotiri and Dhekelia", "Aland Islands", "American Samoa",
    "Bonaire, Saint Eustatius and Saba", "Bouvet Island",
    "British Indian Ocean Territory", "British Virgin Islands",
    "Cayman Islands", "Christmas Island", "Clipperton Island",
    "Cocos Islands", "East Timor", "England", "Faroe Islands",
    "French Guiana", "French Polynesia", "French Southern Territories",
    "Gibraltar", "Greenland", "Guam", "Guernsey",
    "Heard Island and McDonald Islands", "Hong Kong", "Isle of Man",
    "Jersey", "Marshall Islands", "Martinique", "Mayotte", "Montserrat",
    "Nauru", "New Caledonia", "Niue", "Norfolk Island",
    "Northern Mariana Islands", "Paracel Islands", "Pitcairn",
    "Puerto Rico", "Reunion", "Saint Barthelemy", "Saint Helena",
    "Saint Martin", "Saint Pierre and Miquelon", "Sao Tome and Principe",
    "Seychelles", "Sint Maarten", "Svalbard and Jan Mayen", "Swaziland",
    "Taiwan", "Turks and Caicos Islands", "U.S. Virgin Islands",
    "Wallis and Futuna"
  )
  parent <- c(
    "United Kingdom", "Finland", "United States",
    "Netherlands", "Norway",
    "United Kingdom", "United Kingdom",
    "United Kingdom", "Australia", "France",
    "Australia", "Indonesia", "United Kingdom", "Denmark",
    "France", "France", "France",
    "United Kingdom", "Denmark", "United States", "United Kingdom",
    "Australia", "China", "United Kingdom",
    "United Kingdom", "United States", "France", "France", "United Kingdom",
    "Micronesia", "France", "New Zealand", "Australia",
    "United States", "China", "United Kingdom",
    "United States", "France", "France", "United Kingdom",
    "France", "France", "Portugal",
    "France", "Netherlands", "Norway", "South Africa",
    "China", "United Kingdom", "United States",
    "France"
  )
  data.frame(child = child, parent = parent, stringsAsFactors = FALSE)
}

#' Subdivisions the reference treats as states but which are countries
#'
#' Internal.  From \code{gnrs_db/sql/country_as_state.sql} and
#' \code{state_as_county.sql}: the constituent countries of the United
#' Kingdom.  A submission naming one as the country resolves to the United
#' Kingdom with it as the state, and a submitted state under it is then looked
#' for among its counties.
#' @keywords internal
#' @noRd
gnrs_country_as_state_names <- function() {
  c("England", "Scotland", "Wales", "Northern Ireland")
}

#' Alternate state names the upstream build adds by hand
#'
#' Internal.  From \code{gnrs_db/sql/fix_errors_state_province.sql}.  These
#' are inserted before the name-type column exists, so they carry the type of
#' the GeoNames names and take part in exact alternate-name matching.
#' @keywords internal
#' @noRd
gnrs_extra_state_names <- function() {
  data.frame(
    country_iso = c("CU", "PE", "PA", "PA"),
    state_province_ascii = c(
      "La Habana", "Provincia de Lima", "Embera-Wounaan", "Embera-Wounaan"
    ),
    name = c(
      "Ciudad de la Habana", "Lima Province", "Embera",
      paste0("Ember", intToUtf8(0x00E1))
    ),
    stringsAsFactors = FALSE
  )
}

#' Load the local reference data, building the lookup structures
#'
#' Internal.  Read once per session and held in \code{gnrs_env}; reread when
#' the files on disk change.  Besides the tables as stored, the loaded backbone
#' carries lower-cased columns and the composite keys the exact-match steps
#' look up with \code{match()}, so that a step over a whole batch is one
#' vectorised lookup.  Every table is ordered by identifier, which is what
#' makes \code{match()} return the lowest identifier where the upstream SQL
#' would pick one of several matches arbitrarily.
#'
#' @param dir Cache directory.
#' @param quiet Suppress the loading message?
#' @return The backbone, a list.
#' @keywords internal
#' @noRd
gnrs_backbone <- function(dir = gnrs_cache_dir(), quiet = FALSE) {
  paths <- c(
    gnrs_reference_path(c("country", "state_province", "county_parish"), dir),
    gnrs_names_path(dir)
  )
  if (!all(file.exists(paths))) {
    stop(
      "The local reference data has not been built. Run GNRS_local_build().",
      call. = FALSE
    )
  }
  key <- paste(normalizePath(dir, mustWork = FALSE), paste(file.mtime(paths), collapse = "|"))
  if (identical(gnrs_env$key, key) && !is.null(gnrs_env$backbone)) {
    return(gnrs_env$backbone)
  }

  if (!quiet) message("Loading the local GNRS reference data ...")

  country <- as.data.frame(nanoparquet::read_parquet(paths[1]))
  state <- as.data.frame(nanoparquet::read_parquet(paths[2]))
  county <- as.data.frame(nanoparquet::read_parquet(paths[3]))
  names <- as.data.frame(nanoparquet::read_parquet(paths[4]))

  country <- country[order(country$country_id), , drop = FALSE]
  state <- state[order(state$state_province_id), , drop = FALSE]
  county <- county[order(county$county_parish_id), , drop = FALSE]
  names <- names[order(names$id, names$name), , drop = FALSE]
  rownames(country) <- rownames(state) <- rownames(county) <- rownames(names) <- NULL

  sep <- "\r"
  k <- function(parent, x) {
    out <- paste0(parent, sep, x)
    out[is.na(x) | !nzchar(x)] <- NA_character_
    out
  }

  country$lower <- gnrs_lower(country$country)

  state$lower_name <- gnrs_lower(state$state_province)
  state$lower_ascii <- gnrs_lower(state$state_province_ascii)
  state$lower_std <- gnrs_lower(state$state_province_std)
  state$key_name <- k(state$country_id, state$state_province)
  state$key_lower_name <- k(state$country_id, state$lower_name)
  state$key_ascii <- k(state$country_id, state$state_province_ascii)
  state$key_lower_ascii <- k(state$country_id, state$lower_ascii)
  state$key_std <- k(state$country_id, state$state_province_std)
  state$key_lower_std <- k(state$country_id, state$lower_std)
  state$key_code_full <- k(state$country_id, state$state_province_code_full)
  state$key_hasc_full <- k(state$country_id, state$hasc_full)
  state$key_code2_full <- k(state$country_id, state$state_province_code2_full)
  state$key_code <- k(state$country_id, state$state_province_code)
  state$key_hasc <- k(state$country_id, state$hasc)
  state$key_code2 <- k(state$country_id, state$state_province_code2)

  county$lower_name <- gnrs_lower(county$county_parish)
  county$lower_ascii <- gnrs_lower(county$county_parish_ascii)
  county$lower_std <- gnrs_lower(county$county_parish_std)
  county$key_name <- k(county$state_province_id, county$county_parish)
  county$key_lower_name <- k(county$state_province_id, county$lower_name)
  county$key_ascii <- k(county$state_province_id, county$county_parish_ascii)
  county$key_lower_ascii <- k(county$state_province_id, county$lower_ascii)
  county$key_std <- k(county$state_province_id, county$county_parish_std)
  county$key_lower_std <- k(county$state_province_id, county$lower_std)
  county$key_code_full <- k(county$state_province_id, county$county_parish_code_full)
  county$key_hasc_full <- k(county$state_province_id, county$hasc_2_full)
  county$key_code2_full <- k(county$state_province_id, county$county_parish_code2_full)
  county$key_code <- k(county$state_province_id, county$county_parish_code)
  county$key_hasc <- k(county$state_province_id, county$hasc_2)
  county$key_code2 <- k(county$state_province_id, county$county_parish_code2)

  # The name table, split by level, each with its parent identifier so that
  # scoped lookups need no join at query time
  names$lower <- gnrs_lower(names$name)
  names$original <- names$name_type == "original from geonames"

  country_names <- names[names$level == "country", , drop = FALSE]
  state_names <- names[names$level == "state_province", , drop = FALSE]
  county_names <- names[names$level == "county_parish", , drop = FALSE]
  state_names$country_id <- state$country_id[match(state_names$id, state$state_province_id)]
  county_names$state_province_id <- county$state_province_id[
    match(county_names$id, county$county_parish_id)
  ]
  state_names$key_original <- ifelse(
    state_names$original, k(state_names$country_id, state_names$name), NA_character_
  )
  county_names$key_original <- ifelse(
    county_names$original, k(county_names$state_province_id, county_names$name), NA_character_
  )
  rownames(country_names) <- rownames(state_names) <- rownames(county_names) <- NULL

  backbone <- list(
    country = country,
    state = state,
    county = county,
    country_names = country_names,
    state_names = state_names,
    county_names = county_names,
    # Row positions grouped by parent, for scoped steps
    state_rows_by_country = split(seq_len(nrow(state)), state$country_id),
    county_rows_by_state = split(seq_len(nrow(county)), county$state_province_id),
    country_name_rows_by_id = split(seq_len(nrow(country_names)), country_names$id),
    state_name_rows_by_country = split(seq_len(nrow(state_names)), state_names$country_id),
    state_name_rows_by_id = split(seq_len(nrow(state_names)), state_names$id),
    county_name_rows_by_state = split(seq_len(nrow(county_names)), county_names$state_province_id),
    # Per-scope trigram indexes and name sets, built on first use
    cache = new.env(parent = emptyenv()),
    sep = sep
  )

  gnrs_env$key <- key
  gnrs_env$backbone <- backbone
  backbone
}

#' A scope of reference names to match against, cached
#'
#' Internal.  Each fuzzy or wildcard step in the SQL compares the submitted
#' value with one column of the reference rows in scope: the states of one
#' country, the counties of one state, or the alternate names of either.  This
#' returns that column with the identifiers it belongs to and, for the fuzzy
#' steps, its trigram index, building each once per session.
#'
#' @param bb The backbone.
#' @param what Which scope; see the switch.
#' @param parent Parent identifier where the scope is nested.
#' @param index Build the trigram index as well?
#' @return A list with \code{ids}, \code{names} and, when asked for,
#'   \code{index}.
#' @keywords internal
#' @noRd
gnrs_scope <- function(bb, what, parent = NA, index = FALSE) {
  key <- paste(what, parent, index, sep = "|")
  cached <- bb$cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  pick <- function(tbl, rows, id_col, name_col, filter = NULL) {
    if (!is.null(filter)) rows <- rows[filter[rows]]
    list(ids = tbl[[id_col]][rows], names = tbl[[name_col]][rows])
  }
  st <- bb$state
  ct <- bb$county
  co <- bb$country

  scope <- switch(what,
    # Every country, by its standard name
    country_std = pick(co, seq_len(nrow(co)), "country_id", "country"),
    # Every country's GeoNames alternate names
    country_alt = pick(bb$country_names, which(bb$country_names$original), "id", "name"),
    # Countries that are subdivisions of the given country, by standard name
    sac_std = pick(co, which(!is.na(co$alt_country_id) & co$alt_country_id == parent), "country_id", "country"),
    # Their alternate names, of every type
    sac_alt = {
      ids <- co$country_id[!is.na(co$alt_country_id) & co$alt_country_id == parent]
      rows <- unlist(bb$country_name_rows_by_id[as.character(ids)], use.names = FALSE)
      pick(bb$country_names, rows, "id", "name")
    },
    # The states of one country, by each name column
    state_name = pick(st, bb$state_rows_by_country[[as.character(parent)]], "state_province_id", "state_province"),
    state_ascii = pick(st, bb$state_rows_by_country[[as.character(parent)]], "state_province_id", "state_province_ascii"),
    state_std = pick(st, bb$state_rows_by_country[[as.character(parent)]], "state_province_id", "state_province_std"),
    # Their alternate names: all types, or GeoNames only
    state_alt = pick(bb$state_names, bb$state_name_rows_by_country[[as.character(parent)]], "id", "name"),
    state_alt_original = pick(bb$state_names, bb$state_name_rows_by_country[[as.character(parent)]], "id", "name", bb$state_names$original),
    # The states that are really countries (England and the rest), by column
    cas_name = pick(st, which(st$is_countryasstate), "state_province_id", "state_province"),
    cas_ascii = pick(st, which(st$is_countryasstate), "state_province_id", "state_province_ascii"),
    cas_std = pick(st, which(st$is_countryasstate), "state_province_id", "state_province_std"),
    cas_alt = {
      ids <- st$state_province_id[st$is_countryasstate]
      rows <- unlist(bb$state_name_rows_by_id[as.character(ids)], use.names = FALSE)
      pick(bb$state_names, rows, "id", "name")
    },
    # The counties of one state, by column
    county_name = pick(ct, bb$county_rows_by_state[[as.character(parent)]], "county_parish_id", "county_parish"),
    county_ascii = pick(ct, bb$county_rows_by_state[[as.character(parent)]], "county_parish_id", "county_parish_ascii"),
    county_std = pick(ct, bb$county_rows_by_state[[as.character(parent)]], "county_parish_id", "county_parish_std"),
    county_alt = pick(bb$county_names, bb$county_name_rows_by_state[[as.character(parent)]], "id", "name"),
    county_alt_original = pick(bb$county_names, bb$county_name_rows_by_state[[as.character(parent)]], "id", "name", bb$county_names$original),
    # Counties of one state that are really states (the counties of England)
    sac_county_name = pick(ct, bb$county_rows_by_state[[as.character(parent)]], "county_parish_id", "county_parish", ct$is_stateascounty),
    sac_county_ascii = pick(ct, bb$county_rows_by_state[[as.character(parent)]], "county_parish_id", "county_parish_ascii", ct$is_stateascounty),
    sac_county_std = pick(ct, bb$county_rows_by_state[[as.character(parent)]], "county_parish_id", "county_parish_std", ct$is_stateascounty),
    sac_county_alt = {
      rows <- bb$county_rows_by_state[[as.character(parent)]]
      ids <- ct$county_parish_id[rows[ct$is_stateascounty[rows]]]
      keep <- bb$county_name_rows_by_state[[as.character(parent)]]
      keep <- keep[bb$county_names$id[keep] %in% ids]
      pick(bb$county_names, keep, "id", "name")
    },
    stop("unknown scope: ", what)
  )

  if (is.null(scope$ids)) {
    scope <- list(ids = integer(0), names = character(0))
  }
  # Empty names would match everything in a wildcard step
  keep <- !is.na(scope$names) & nzchar(scope$names)
  scope$ids <- scope$ids[keep]
  scope$names <- scope$names[keep]

  if (index) {
    scope$index <- gnrs_trigram_index(scope$names)
  }

  bb$cache[[key]] <- scope
  scope
}
