#' Standardize political division names without an internet connection
#'
#' Resolves country, state/province and county/parish names against a locally
#' cached copy of the GNRS reference data, using the same matching cascade as
#' the web service.  Run \code{GNRS_local_build()} once to download and prepare
#' the data; afterwards this function needs no internet connection, no round
#' trips to the server, and has no limit on the number of rows.
#'
#' @param political_division_dataframe A properly formatted data.frame with the
#'   columns \code{user_id}, \code{country}, \code{state_province} and
#'   \code{county_parish}; see \code{GNRS_template()}.  The same input as
#'   \code{GNRS()}.
#' @param threshold Fuzzy-match threshold, between 0 and 1: the similarity a
#'   reference name must reach for a fuzzy match to be accepted.  Defaults to
#'   0.5, the web service's default.
#' @param alternate_names Use the GeoNames alternate names?  Defaults to TRUE,
#'   which is what the web service does; it needs the \code{"geonames"}
#'   component to have been built.  With FALSE, matching is confined to the
#'   standard names and codes, which is quicker to set up but misses names in
#'   other languages, abbreviations and many spelling variants.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param build_missing Should a component that has not been built yet be
#'   downloaded and built now?  Defaults to \code{interactive()}, which reports
#'   the size and asks first.  In a script it is therefore FALSE and nothing is
#'   downloaded silently: the function reports what is missing and the call that
#'   would fix it.  Set it to TRUE to allow an unattended build.
#' @param quiet Suppress progress messages?
#' @return A data.frame with the same columns as \code{GNRS()}, one row per
#'   input row in input order.  Scores are numeric rather than the character
#'   strings the web service returns; identifiers are character, as the web
#'   service returns them; empty values are "".
#' @note \strong{This is a new implementation and should be treated as beta.}
#'   It is a port of the SQL the web service runs, resolving against the
#'   service's own reference tables, so identifiers, standard names and codes
#'   agree with the service's.  Measured against the web service on the
#'   package's validation sets it returns the same resolved political
#'   divisions for the large majority of rows; \code{vignette("GNRS_offline")}
#'   reports the figures and explains the differences.  Beyond those sets it
#'   has not been tested broadly, nor independently reviewed, so for work where
#'   the answer matters check a sample against \code{GNRS()} and please report
#'   what disagrees.
#' @note Results are not guaranteed to be identical to \code{GNRS()}.  The
#'   alternate names are downloaded from GeoNames directly and are newer than
#'   the service's copy, so a name may match locally that the service misses,
#'   or vice versa; where the service would choose arbitrarily among equally
#'   good matches, this function chooses the lowest identifier; and the
#'   service caches earlier answers, which this function does not.  See the
#'   vignette for the full list.
#' @note The web service's own defaults apply: alternate names are used and the
#'   fuzzy threshold is 0.5.
#' @seealso \code{\link{GNRS_local_build}}, \code{\link{GNRS_local_status}},
#'   \code{\link{GNRS_local_citations}}, \code{\link{GNRS}} for the web service.
#' @export
#' @examples \dontrun{
#' # One-off setup
#' GNRS_local_build()
#'
#' results <- GNRS_local(gnrs_testfile)
#'
#' template <- GNRS_template(nrow = 2)
#' template$country <- c("United Stapes", "Mexico")
#' template$state_province <- c("Arizona", "Sinalo")
#' GNRS_local(template)
#' }
GNRS_local <- function(political_division_dataframe,
                       threshold = 0.5,
                       alternate_names = TRUE,
                       dir = gnrs_cache_dir(),
                       build_missing = interactive(),
                       quiet = FALSE) {
  if (!inherits(political_division_dataframe, "data.frame")) {
    stop("political_division_dataframe should be a data.frame", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
    threshold < 0 || threshold > 1) {
    stop("threshold should be a single number between 0 and 1", call. = FALSE)
  }
  if (!is.logical(alternate_names) || length(alternate_names) != 1L || is.na(alternate_names)) {
    stop("alternate_names should be TRUE or FALSE", call. = FALSE)
  }

  input <- gnrs_check_input(political_division_dataframe)

  sources <- if (alternate_names) c("gnrs", "geonames") else "gnrs"
  if (!gnrs_require_sources(sources, dir = dir, build_missing = build_missing, quiet = quiet)) {
    return(invisible(NULL))
  }
  if (!alternate_names && gnrs_is_built("geonames", dir)) {
    # Built but not wanted: the name table on disk includes the GeoNames
    # names, so they are set aside when loading
    bb <- gnrs_backbone(dir, quiet = quiet)
    bb <- gnrs_without_alternate_names(bb)
  } else {
    bb <- gnrs_backbone(dir, quiet = quiet)
  }
  # The assignment helpers read the backbone from the session cache, so a
  # backbone with the alternate names set aside must be what they see
  old <- gnrs_env$backbone
  gnrs_env$backbone <- bb
  on.exit(gnrs_env$backbone <- old, add = TRUE)

  # Resolution depends only on the three submitted names, so each distinct
  # combination is resolved once
  key <- paste(input$country, input$state_province, input$county_parish, sep = "\r")
  distinct <- !duplicated(key)
  u <- data.frame(
    country_verbatim = input$country[distinct],
    state_province_verbatim = input$state_province[distinct],
    county_parish_verbatim = input$county_parish[distinct],
    stringsAsFactors = FALSE
  )

  if (!quiet && nrow(u) > 0) {
    message("Resolving ", nrow(u), " distinct political division", if (nrow(u) == 1) "" else "s", " ...")
  }
  resolved <- gnrs_resolve(u, bb, threshold = threshold)

  m <- match(key, key[distinct])
  gnrs_build_output(resolved[m, , drop = FALSE], input, threshold)
}

#' Check the submitted data.frame and put it in a standard form
#'
#' Internal.  The same checks as \code{GNRS()}: a data.frame with the four
#' template columns, and identifiers that are either all missing (in which case
#' they are numbered) or unique.  Missing names become "".
#'
#' @return A data.frame of \code{user_id}, \code{country}, \code{state_province},
#'   \code{county_parish}, all character.
#' @keywords internal
#' @noRd
gnrs_check_input <- function(df) {
  wanted <- c("user_id", "country", "state_province", "county_parish")
  present <- wanted %in% names(df)
  if (!all(present[-1])) {
    stop(
      "political_division_dataframe should have the columns country, ",
      "state_province and county_parish (see GNRS_template()); missing: ",
      paste(wanted[-1][!present[-1]], collapse = ", "),
      call. = FALSE
    )
  }
  if (!"user_id" %in% names(df)) {
    df$user_id <- NA
  }
  n <- nrow(df)
  ids <- df$user_id
  if (n > 0 && all(is.na(ids))) {
    ids <- seq_len(n)
  }
  ids <- as.character(ids)
  if (any(duplicated(ids))) {
    stop("user_id should be either null or populated by unique values", call. = FALSE)
  }

  as_name <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    enc2utf8(x)
  }
  data.frame(
    user_id = ids,
    country = as_name(df$country),
    state_province = as_name(df$state_province),
    county_parish = as_name(df$county_parish),
    stringsAsFactors = FALSE
  )
}

#' A copy of the backbone with the GeoNames alternate names set aside
#'
#' Internal.  Keeps the names the upstream build adds from the reference
#' tables themselves (standard names, codes, GADM names) and drops the ones
#' that came from GeoNames.  A GeoNames division's own name and ASCII name are
#' typed as GeoNames names in the table; they are retained, since they are
#' there whether or not the alternate names were downloaded.
#' @keywords internal
#' @noRd
gnrs_without_alternate_names <- function(bb) {
  keep_country <- !bb$country_names$original |
    bb$country_names$name %in% bb$country$country
  st <- bb$state
  own_state <- paste(st$state_province_id, c(st$state_province, st$state_province_ascii), sep = "\r")
  keep_state <- !bb$state_names$original |
    paste(bb$state_names$id, bb$state_names$name, sep = "\r") %in% own_state
  ct <- bb$county
  own_county <- paste(ct$county_parish_id, c(ct$county_parish, ct$county_parish_ascii), sep = "\r")
  keep_county <- !bb$county_names$original |
    paste(bb$county_names$id, bb$county_names$name, sep = "\r") %in% own_county

  bb$country_names <- bb$country_names[keep_country, , drop = FALSE]
  bb$state_names <- bb$state_names[keep_state, , drop = FALSE]
  bb$county_names <- bb$county_names[keep_county, , drop = FALSE]
  bb$country_name_rows_by_id <- split(seq_len(nrow(bb$country_names)), bb$country_names$id)
  bb$state_name_rows_by_country <- split(seq_len(nrow(bb$state_names)), bb$state_names$country_id)
  bb$state_name_rows_by_id <- split(seq_len(nrow(bb$state_names)), bb$state_names$id)
  bb$county_name_rows_by_state <- split(seq_len(nrow(bb$county_names)), bb$county_names$state_province_id)
  bb$cache <- new.env(parent = emptyenv())
  bb
}

#' Assemble the output in the web service's layout
#'
#' Internal.  The columns, their order and their empty-value conventions are
#' those of the web service's response, so that code written for
#' \code{GNRS()} reads the result.
#' @keywords internal
#' @noRd
gnrs_build_output <- function(r, input, threshold) {
  chr <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }
  poldiv_full <- paste(
    trimws(input$country), trimws(input$state_province), trimws(input$county_parish),
    sep = "@"
  )
  out <- data.frame(
    poldiv_full = poldiv_full,
    country_verbatim = input$country,
    state_province_verbatim = input$state_province,
    state_province_verbatim_alt = chr(r$state_province_verbatim_alt),
    county_parish_verbatim = input$county_parish,
    county_parish_verbatim_alt = chr(r$county_parish_verbatim_alt),
    country = chr(r$country),
    state_province = chr(r$state_province),
    county_parish = chr(r$county_parish),
    country_id = chr(r$country_id),
    state_province_id = chr(r$state_province_id),
    county_parish_id = chr(r$county_parish_id),
    country_iso = chr(r$country_iso),
    state_province_iso = chr(r$state_province_iso),
    county_parish_iso = chr(r$county_parish_iso),
    geonameid = chr(r$geonameid),
    gid_0 = chr(r$gid_0),
    gid_1 = chr(r$gid_1),
    gid_2 = chr(r$gid_2),
    match_method_country = chr(r$match_method_country),
    match_method_state_province = chr(r$match_method_state_province),
    match_method_county_parish = chr(r$match_method_county_parish),
    match_score_country = r$match_score_country,
    match_score_state_province = r$match_score_state_province,
    match_score_county_parish = r$match_score_county_parish,
    threshold_fuzzy = rep(threshold, nrow(r)),
    overall_score = r$overall_score,
    poldiv_submitted = chr(r$poldiv_submitted),
    poldiv_matched = chr(r$poldiv_matched),
    match_status = chr(r$match_status),
    user_id = input$user_id,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}
