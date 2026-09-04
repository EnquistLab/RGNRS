#' Build the local GNRS reference data
#'
#' Downloads the reference data the Geographic Name Resolution Service resolves
#' against and prepares it for offline use.  This needs to be done once;
#' afterwards \code{GNRS_local()} works with no internet connection.  The data
#' is kept in the standard user cache directory, and can be removed again with
#' \code{GNRS_local_remove()}.
#'
#' Two components are fetched by default, and two more can be added.
#' \code{"gnrs"} is the web service's own
#' reference tables of countries, states/provinces and counties/parishes,
#' fetched through its API in a few small requests: every political division it
#' knows, with the identifiers, standard names, codes and GADM identifiers it
#' returns.  Because the local copy is the service's own reference, local results
#' carry the same identifiers as results from the web service.
#' \code{"geonames"} is the alternate-names file from GeoNames, about 195 MB,
#' from which the service takes the names in other languages, abbreviations and
#' historical names its alternate-name matching uses; the API does not serve
#' these, so they are fetched from the publisher and only the rows for the
#' reference political divisions are kept, a few megabytes.  Without it,
#' matching is confined to standard names and codes, so it is built by default.
#'
#' \code{"gadm"} lays the current release of GADM over the service's tables.
#' The service's reference was built from GADM 3.6 in 2020 and grows more out
#' of date as time passes; this component fetches the current GADM divisions
#' from the publisher (the world GeoPackage, about 1.4 GB, of which only the
#' names and codes of levels 0 to 2 are kept; reading it needs the RSQLite
#' package), links each to the service's
#' division it corresponds to, by GADM identifier, HASC code or name, and adds
#' those with no counterpart as new divisions.  Linked divisions keep the
#' service's identifiers, so results remain joinable with the service's; new
#' ones are numbered above every existing identifier, as the service's own
#' build numbers its GADM additions, and are local to your build.  The
#' \code{gid_0}, \code{gid_1} and \code{gid_2} columns then refer to the
#' GADM version built, and GADM's names and alternate names take part in
#' matching.  It is not built by default because it changes what a result
#' means: build it when you want current GADM identifiers or coverage of
#' divisions the service lacks, and remove it again with
#' \code{GNRS_local_remove(sources = "gadm")}.
#'
#' \code{"points"} is GeoNames' own latitude and longitude for every reference
#' division, taken from its full gazetteer (about 400 MB, of which under a
#' megabyte is kept).  It serves one purpose: when it has been built and the
#' sf package is installed, the \code{"gadm"} build checks every link it
#' makes against the geometry, measuring the distance from the division's
#' point to the polygon it was linked to, and withdraws a link that rests on
#' an identifier or a code alone when the point lies more than 25 km away.
#' A link whose names agree outright is kept whatever the point says, since
#' GeoNames places its points near an edge often enough.  Build it before
#' \code{"gadm"} (the order is arranged whatever order is given).
#'
#' Each component is recorded with its version, so that results obtained locally
#' can be cited as precisely as results from the web service.  Use
#' \code{GNRS_local_status()} to see what has been built.
#'
#' @param sources Character vector of components to build: \code{"gnrs"},
#'   \code{"geonames"} (the two defaults), \code{"points"} and
#'   \code{"gadm"}.  The other components are layered on or filtered to the
#'   service's tables, so \code{"gnrs"} is built first if it is missing
#'   whatever is asked for.
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param overwrite Re-download and rebuild even if the data is already present?
#' @param keep_archive Keep the downloaded GeoNames and GADM archives after
#'   building?  They are not needed for matching and are far larger than what
#'   is kept from them, so they are deleted by default.  Keep them if you
#'   expect to rebuild.
#' @param url URL of the GNRS API, from which the reference tables are fetched.
#' @param quiet Suppress progress messages?
#' @return The output of \code{GNRS_local_status()}, invisibly.
#' @note The reference tables come from the web service, whose data is in turn
#'   built from GADM, GeoNames and Natural Earth; the alternate names come from
#'   GeoNames.  Please cite them alongside the GNRS;
#'   \code{GNRS_local_citations()} assembles the citations.
#' @seealso \code{\link{GNRS_local}}, \code{\link{GNRS_local_status}}
#' @export
#' @examples \dontrun{
#' # One-off setup; needs an internet connection and a few minutes
#' GNRS_local_build()
#'
#' # The reference tables alone: quick, but no alternate names
#' GNRS_local_build("gnrs")
#'
#' # Add the current GADM release on top of the service's tables
#' GNRS_local_build("gadm")
#'
#' # See what is available offline
#' GNRS_local_status()
#' }
GNRS_local_build <- function(sources = c("gnrs", "geonames"),
                             dir = gnrs_cache_dir(create = TRUE),
                             overwrite = FALSE,
                             keep_archive = FALSE,
                             url = "https://gnrsapi.xyz/gnrs_api.php",
                             quiet = FALSE) {
  registry <- gnrs_builtin_registry()
  unknown <- setdiff(sources, names(registry))
  if (length(unknown) > 0) {
    message(
      "Unknown source(s): ", paste(unknown, collapse = ", "),
      ". Options are: ", paste(names(registry), collapse = ", ")
    )
    return(invisible(NULL))
  }

  if (!"gnrs" %in% sources && !gnrs_is_built("gnrs", dir)) {
    if (!quiet) {
      message("The other components are layered on the service's reference tables, so 'gnrs' is built first.")
    }
    sources <- c("gnrs", sources)
  }
  # The reference tables come first whatever order was given, then the
  # alternate names and the coordinates, then the GADM layer, whose linking
  # uses the alternate names and the coordinates when it has them
  sources <- intersect(c("gnrs", "geonames", "points", "gadm"), sources)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (source in sources) {
    if (gnrs_is_built(source, dir) && !overwrite) {
      if (!quiet) message("Source '", source, "' is already built; skipping.")
      if (source == "geonames") {
        gnrs_tidy_archive(dir = dir, keep_archive = keep_archive, quiet = quiet)
      }
      if (source == "gadm") {
        gnrs_tidy_gadm_archive(dir = dir, keep_archive = keep_archive, quiet = quiet)
      }
      if (source == "points") {
        gnrs_tidy_points_archive(dir = dir, keep_archive = keep_archive, quiet = quiet)
      }
      reference <- gnrs_reference_path(c("country", "state_province", "county_parish"), dir)
      if (source %in% c("gnrs", "gadm") && !all(file.exists(reference))) {
        gnrs_finalize_reference(dir = dir, quiet = quiet)
      }
      next
    }

    if (source == "gnrs") {
      gnrs_build_reference(dir = dir, url = url, quiet = quiet)
    } else if (source == "gadm") {
      gnrs_build_gadm(dir = dir, overwrite = overwrite, keep_archive = keep_archive, quiet = quiet)
    } else if (source == "points") {
      gnrs_build_points(dir = dir, overwrite = overwrite, keep_archive = keep_archive, quiet = quiet)
    } else {
      gnrs_build_altnames(dir = dir, overwrite = overwrite, keep_archive = keep_archive, quiet = quiet)
    }
    # The reference tables are derived from the snapshot and the GADM layer,
    # whose linking reads the alternate names, so they are rederived after
    # any of the three
    if (source %in% c("gnrs", "gadm") || gnrs_is_built("gadm", dir)) {
      gnrs_finalize_reference(dir = dir, quiet = quiet)
    }
    if (!quiet) message("Built '", source, "'.")
  }

  # The name table draws on every component, so it is reassembled after any
  # build
  if (gnrs_is_built("gnrs", dir)) {
    gnrs_assemble_names(dir = dir, quiet = quiet)
  }
  gnrs_forget_backbone()

  status <- suppressMessages(GNRS_local_status(dir))
  if (!quiet && any(status$built)) {
    message(
      "\nLocal reference data ready. Please cite the sources listed by ",
      "GNRS_local_citations()."
    )
  }
  invisible(status)
}

#' Fetch the reference tables from the GNRS API and store them
#'
#' Internal.  Four requests: the version, and the complete country, state and
#' county lists.  The lists are the tables the service resolves against, less
#' the alternate names.  Columns the service does not serve but the resolution
#' steps use, such as the short codes, are derived here as the upstream build
#' derives them.
#'
#' @param dir Cache directory.
#' @param url API URL.
#' @param quiet Suppress progress messages?
#' @return The provenance record, invisibly.
#' @keywords internal
#' @noRd
gnrs_build_reference <- function(dir, url, quiet = FALSE) {
  if (!quiet) message("Fetching the GNRS reference tables from ", url, " ...")

  fetch <- function(mode, data_json = NULL, what) {
    out <- suppressMessages(gnrs_core(url = url, mode = mode, data_json = data_json))
    if (is.null(out) || nrow(out) == 0) {
      stop(
        "Could not fetch the ", what, " from the GNRS API at ", url,
        ". The service may be down; GNRS_version() reports whether it can be reached.",
        call. = FALSE
      )
    }
    out
  }

  version <- fetch("meta", what = "version information")
  countries <- fetch("countrylist", what = "country list")
  if (!quiet) message("  ", nrow(countries), " countries")
  states <- fetch("statelist", jsonlite::toJSON(data.frame(country = "")), "state list")
  if (!quiet) message("  ", nrow(states), " states/provinces")
  counties <- fetch("countylist", jsonlite::toJSON(data.frame(state = "")), "county list")
  if (!quiet) message("  ", nrow(counties), " counties/parishes")

  tables <- gnrs_import_reference(countries, states, counties)

  nanoparquet::write_parquet(tables$country, gnrs_snapshot_path("country", dir), compression = "gzip")
  nanoparquet::write_parquet(tables$state, gnrs_snapshot_path("state_province", dir), compression = "gzip")
  nanoparquet::write_parquet(tables$county, gnrs_snapshot_path("county_parish", dir), compression = "gzip")

  provenance <- list(
    source = "gnrs",
    full_name = gnrs_builtin_registry()$gnrs$full_name,
    version = paste0(
      "database ", version$db_version[1], " (", version$db_version_build_date[1],
      "), code ", version$code_version[1]
    ),
    db_version = version$db_version[1],
    db_version_build_date = version$db_version_build_date[1],
    code_version = version$code_version[1],
    url = url,
    downloaded = as.character(Sys.Date()),
    n_country = nrow(tables$country),
    n_state_province = nrow(tables$state),
    n_county_parish = nrow(tables$county),
    geonames_max_id = tables$geonames_max_id
  )
  saveRDS(provenance, gnrs_provenance_path("gnrs", dir))
  invisible(provenance)
}

#' Derive the reference tables from the snapshot and the GADM layer
#'
#' Internal.  The tables the resolver reads are the API snapshot as fetched,
#' with the current GADM divisions laid over it where that component has been
#' built.  Rederived whenever either component changes.  The names GADM
#' contributes are written alongside for the name-table assembly, and the link
#' counts are recorded in the GADM provenance so that the status report can
#' say what the layer did.
#'
#' @param dir Cache directory.
#' @param quiet Suppress progress messages?
#' @return Invisibly, the list of tables.
#' @keywords internal
#' @noRd
gnrs_finalize_reference <- function(dir = gnrs_cache_dir(), quiet = FALSE) {
  country <- as.data.frame(nanoparquet::read_parquet(gnrs_snapshot_path("country", dir)))
  state <- as.data.frame(nanoparquet::read_parquet(gnrs_snapshot_path("state_province", dir)))
  county <- as.data.frame(nanoparquet::read_parquet(gnrs_snapshot_path("county_parish", dir)))

  gadm_names <- NULL
  if (gnrs_is_built("gadm", dir)) {
    if (!quiet) message("Applying the GADM layer to the reference tables ...")
    gadm <- as.data.frame(nanoparquet::read_parquet(gnrs_gadm_path(dir)))
    altnames <- if (file.exists(gnrs_altnames_path(dir))) {
      as.data.frame(nanoparquet::read_parquet(gnrs_altnames_path(dir)))
    }
    distances <- if (file.exists(gnrs_gadm_distances_path(dir))) {
      as.data.frame(nanoparquet::read_parquet(gnrs_gadm_distances_path(dir)))
    }
    applied <- gnrs_apply_gadm(country, state, county, gadm, altnames = altnames, distances = distances)
    country <- applied$country
    state <- applied$state
    county <- applied$county
    gadm_names <- applied$names
    record_path <- gnrs_provenance_path("gadm", dir)
    if (file.exists(record_path)) {
      record <- readRDS(record_path)
      record$counts <- applied$counts
      record$steps <- applied$steps
      saveRDS(record, record_path)
    }
    if (!quiet) {
      k <- applied$counts
      message(
        "  linked ", k$country_linked, " countries, ", k$state_linked, " states and ",
        k$county_linked, " counties; added ", k$country_added, ", ", k$state_added,
        " and ", k$county_added, " that the service lacks"
      )
    }
  }

  nanoparquet::write_parquet(country, gnrs_reference_path("country", dir), compression = "gzip")
  nanoparquet::write_parquet(state, gnrs_reference_path("state_province", dir), compression = "gzip")
  nanoparquet::write_parquet(county, gnrs_reference_path("county_parish", dir), compression = "gzip")
  if (is.null(gadm_names)) {
    unlink(gnrs_gadm_names_path(dir))
  } else {
    nanoparquet::write_parquet(gadm_names, gnrs_gadm_names_path(dir), compression = "gzip")
  }
  gnrs_forget_backbone()
  invisible(list(country = country, state = state, county = county))
}

#' Turn the API's lists into the reference tables the resolver uses
#'
#' Internal.  The API serves the full codes; the short ISO and alternate codes
#' the exact-match steps also test are their last segment.  Short HASC codes
#' are not derived: upstream they come from a legacy table whose join
#' evidently never populated the deployed database (the web service matches
#' "BC" under Canada as an alternate code, not a HASC code, though the legacy
#' table carries it), so the full HASC code is the only one the service
#' matches on.
#'
#' Which divisions came from GeoNames and which were added from GADM is not
#' served either, but the upstream build numbers the GADM additions from one
#' above the highest GeoNames identifier, and the countries it adds are the
#' ones without an ISO code, so the boundary is recovered from them.  The
#' distinction matters for the name table: the upstream build adds the codes of
#' a division to its names only for the GeoNames divisions.
#'
#' @param countries,states,counties The API's lists, as data.frames.
#' @return A list of the three tables and the GeoNames identifier boundary.
#' @keywords internal
#' @noRd
gnrs_import_reference <- function(countries, states, counties) {
  chr <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    x
  }
  int <- function(x) suppressWarnings(as.integer(as.character(x)))

  country <- data.frame(
    country_id = int(countries$country_id),
    country = chr(countries$country),
    iso = chr(countries$iso),
    iso_alpha3 = chr(countries$iso_alpha3),
    fips = chr(countries$fips),
    continent_code = chr(countries$continent_code),
    continent = chr(countries$continent),
    gid_0 = chr(countries$gadm_gid_0),
    stringsAsFactors = FALSE
  )

  added <- country$country_id[is.na(country$iso)]
  geonames_max_id <- if (length(added) > 0) min(added) - 1L else .Machine$integer.max
  country$is_geoname <- country$country_id <= geonames_max_id

  links <- gnrs_state_as_country_links()
  country$alt_country_id <- country$country_id[
    match(links$parent[match(country$country, links$child)], country$country)
  ]

  state <- data.frame(
    state_province_id = int(states$state_province_id),
    country_id = int(states$country_id),
    country_iso = chr(states$country_iso),
    country = chr(states$country),
    state_province = chr(states$state_province),
    state_province_ascii = chr(states$state_province_ascii),
    state_province_std = chr(states$state_province_gnrs),
    state_province_code_full = chr(states$iso_3866_1),
    hasc_full = chr(states$hasc),
    state_province_code2_full = chr(states$iso_3866_1_alt),
    gid_0 = chr(states$gadm_gid_0),
    gid_1 = chr(states$gadm_gid_1),
    stringsAsFactors = FALSE
  )
  state$is_geoname <- state$state_province_id <= geonames_max_id
  state$state_province_code <- gnrs_last_segment(state$state_province_code_full)
  state$hasc <- rep(NA_character_, nrow(state))
  state$state_province_code2 <- gnrs_last_segment(state$state_province_code2_full)
  state$is_countryasstate <- state$state_province %in% gnrs_country_as_state_names()

  county <- data.frame(
    county_parish_id = int(counties$county_parish_id),
    country_id = int(counties$country_id),
    country = chr(counties$country),
    country_iso = chr(counties$country_iso),
    state_province_id = int(counties$state_province_id),
    state_province_ascii = chr(counties$state_province_ascii),
    county_parish = chr(counties$county_parish),
    county_parish_ascii = chr(counties$county_parish_ascii),
    county_parish_std = chr(counties$county_parish_gnrs),
    county_parish_code_full = chr(counties$iso_3166_2),
    county_parish_code2_full = chr(counties$iso_3166_2_alt),
    hasc_2_full = chr(counties$hasc2),
    gid_0 = chr(counties$gadm_gid_0),
    gid_1 = chr(counties$gadm_gid_1),
    gid_2 = chr(counties$gadm_gid_2),
    stringsAsFactors = FALSE
  )
  county$is_geoname <- county$county_parish_id <= geonames_max_id
  county$county_parish_code <- gnrs_last_segment(county$county_parish_code_full)
  county$hasc_2 <- rep(NA_character_, nrow(county))
  county$county_parish_code2 <- gnrs_last_segment(county$county_parish_code2_full)
  county$is_stateascounty <- county$state_province_ascii %in% gnrs_country_as_state_names()

  list(
    country = country[order(country$country_id), ],
    state = state[order(state$state_province_id), ],
    county = county[order(county$county_parish_id), ],
    geonames_max_id = geonames_max_id
  )
}

#' Path of the downloaded GeoNames archive
#' @keywords internal
#' @noRd
gnrs_archive_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "geonames-alternateNamesV2.zip")
}

#' Where the GeoNames gazetteer archive is kept
#' @keywords internal
#' @noRd
gnrs_points_archive_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "points-allCountries.zip")
}

#' Download the GeoNames gazetteer and keep the coordinates of the reference
#' divisions
#'
#' Internal.  \code{allCountries.zip} holds every GeoNames feature, about 13
#' million rows; only the latitude and longitude of the reference states and
#' counties are wanted, so it is read in chunks straight from the zip.
#'
#' @param dir Cache directory.
#' @param overwrite Re-download even if the archive is present?
#' @param keep_archive Keep the archive afterwards?
#' @param quiet Suppress progress messages?
#' @return The provenance record, invisibly.
#' @keywords internal
#' @noRd
gnrs_build_points <- function(dir, overwrite = FALSE, keep_archive = FALSE, quiet = FALSE) {
  spec <- gnrs_builtin_registry()$points
  archive <- gnrs_points_archive_path(dir)

  if (file.exists(archive) && !overwrite) {
    if (!quiet) message("Using cached download of the GeoNames gazetteer")
  } else {
    if (!quiet) message("Downloading ", spec$full_name, " (about ", spec$download_mb, " MB) ...")
    partial <- paste0(archive, ".part")
    old <- options(timeout = max(7200, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    status <- utils::download.file(
      url = spec$url, destfile = partial, mode = "wb", quiet = quiet, cacheOK = FALSE
    )
    if (status != 0 || !file.exists(partial)) {
      unlink(partial)
      stop("Download failed for the GeoNames gazetteer.", call. = FALSE)
    }
    file.rename(partial, archive)
  }
  modified <- tryCatch(
    {
      h <- httr::HEAD(spec$url, httr::timeout(30))
      lm <- httr::headers(h)[["last-modified"]]
      if (is.null(lm)) NA_character_ else format(as.Date(httr::parse_http_date(lm)))
    },
    error = function(e) NA_character_
  )

  state <- nanoparquet::read_parquet(gnrs_snapshot_path("state_province", dir))
  county <- nanoparquet::read_parquet(gnrs_snapshot_path("county_parish", dir))
  ids <- unique(c(
    state$state_province_id[state$is_geoname %in% TRUE],
    county$county_parish_id[county$is_geoname %in% TRUE]
  ))
  points <- gnrs_import_points(archive, ids, quiet = quiet)
  nanoparquet::write_parquet(points, gnrs_points_path(dir), compression = "gzip")

  provenance <- list(
    source = "points",
    full_name = spec$full_name,
    version = if (is.na(modified)) as.character(Sys.Date()) else modified,
    url = spec$url,
    license = spec$license,
    publisher = spec$publisher,
    archive = archive,
    archive_kept = TRUE,
    bytes = as.numeric(file.size(archive)),
    md5 = unname(tools::md5sum(archive)),
    downloaded = as.character(Sys.Date()),
    n_points = nrow(points)
  )
  saveRDS(provenance, gnrs_provenance_path("points", dir))
  if (!quiet) message("  ", nrow(points), " of ", length(ids), " reference divisions have coordinates")

  gnrs_tidy_points_archive(dir = dir, keep_archive = keep_archive, quiet = quiet)
  invisible(provenance)
}

#' Read the coordinates of given GeoNames identifiers out of the gazetteer
#'
#' Internal.  Columns of \code{allCountries.txt}: geonameid, name, asciiname,
#' alternatenames, latitude, longitude, ...  Only the first field is looked
#' at until a row is known to be wanted.
#' @keywords internal
#' @noRd
gnrs_import_points <- function(archive, ids, quiet = FALSE) {
  want <- as.character(as.integer(ids))
  con <- unz(archive, "allCountries.txt")
  open(con, "r")
  on.exit(close(con), add = TRUE)
  kept <- list()
  n <- 0
  repeat {
    lines <- readLines(con, n = 1e6, encoding = "UTF-8", warn = FALSE)
    if (length(lines) == 0) break
    n <- n + length(lines)
    first <- sub("\t.*$", "", lines)
    hit <- lines[first %in% want]
    if (length(hit) > 0) kept[[length(kept) + 1]] <- hit
    if (!quiet) message("  read ", format(n, big.mark = ",", scientific = FALSE), " rows, kept ", sum(lengths(kept)))
  }
  lines <- unlist(kept, use.names = FALSE)
  fields <- strsplit(lines, "\t", fixed = TRUE)
  out <- data.frame(
    geonameid = as.integer(vapply(fields, `[`, "", 1)),
    lat = as.numeric(vapply(fields, `[`, "", 5)),
    lon = as.numeric(vapply(fields, `[`, "", 6)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$lat) & !is.na(out$lon), , drop = FALSE]
  out[!duplicated(out$geonameid), , drop = FALSE]
}

#' Delete the gazetteer archive once the coordinates are stored
#' @keywords internal
#' @noRd
gnrs_tidy_points_archive <- function(dir = gnrs_cache_dir(), keep_archive = FALSE, quiet = FALSE) {
  if (isTRUE(keep_archive) || !file.exists(gnrs_points_path(dir))) {
    return(invisible(FALSE))
  }
  archive <- gnrs_points_archive_path(dir)
  if (!file.exists(archive)) {
    return(invisible(FALSE))
  }
  freed <- file.size(archive)
  if (unlink(archive) != 0) {
    return(invisible(FALSE))
  }
  record_path <- gnrs_provenance_path("points", dir)
  if (file.exists(record_path)) {
    record <- readRDS(record_path)
    record$archive <- NA_character_
    record$archive_kept <- FALSE
    saveRDS(record, record_path)
  }
  if (!quiet) {
    message(
      "  removed the ", round(freed / 1024^2, 1), " MB GeoNames gazetteer archive; ",
      "rebuilding 'points' would download it again (keep_archive = TRUE to keep it)."
    )
  }
  invisible(TRUE)
}

#' Download the GeoNames alternate names and keep the rows that matter
#'
#' Internal.  The archive is large and the reference divisions are a tiny part
#' of it, so it is read in chunks straight from the zip and only rows for
#' identifiers in the reference tables are kept.
#'
#' @param dir Cache directory.
#' @param overwrite Re-download even if the archive is present?
#' @param keep_archive Keep the archive afterwards?
#' @param quiet Suppress progress messages?
#' @return The provenance record, invisibly.
#' @keywords internal
#' @noRd
gnrs_build_altnames <- function(dir, overwrite = FALSE, keep_archive = FALSE, quiet = FALSE) {
  spec <- gnrs_builtin_registry()$geonames
  archive <- gnrs_archive_path(dir)

  if (file.exists(archive) && !overwrite) {
    if (!quiet) message("Using cached download of the GeoNames alternate names")
  } else {
    if (!quiet) message("Downloading ", spec$full_name, " (about ", spec$download_mb, " MB) ...")
    partial <- paste0(archive, ".part")
    # R's default of 60 seconds is not enough for a file this size
    old <- options(timeout = max(3600, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    status <- utils::download.file(
      url = spec$url, destfile = partial, mode = "wb", quiet = quiet, cacheOK = FALSE
    )
    if (status != 0 || !file.exists(partial)) {
      unlink(partial)
      stop("Download failed for the GeoNames alternate names.", call. = FALSE)
    }
    file.rename(partial, archive)
  }

  # The file's own date is its version; asked for separately because
  # download.file() does not hand the headers back
  modified <- tryCatch(
    {
      h <- httr::HEAD(spec$url, httr::timeout(30))
      lm <- httr::headers(h)[["last-modified"]]
      if (is.null(lm)) NA_character_ else format(as.Date(httr::parse_http_date(lm)))
    },
    error = function(e) NA_character_
  )

  country <- nanoparquet::read_parquet(gnrs_reference_path("country", dir))
  state <- nanoparquet::read_parquet(gnrs_reference_path("state_province", dir))
  county <- nanoparquet::read_parquet(gnrs_reference_path("county_parish", dir))
  ids <- unique(c(country$country_id, state$state_province_id, county$county_parish_id))

  alt <- gnrs_import_altnames(archive, ids, quiet = quiet)
  nanoparquet::write_parquet(alt, gnrs_altnames_path(dir), compression = "gzip")

  provenance <- list(
    source = "geonames",
    full_name = spec$full_name,
    version = if (is.na(modified)) as.character(Sys.Date()) else modified,
    url = spec$url,
    license = spec$license,
    publisher = spec$publisher,
    archive = archive,
    archive_kept = TRUE,
    bytes = as.numeric(file.size(archive)),
    md5 = unname(tools::md5sum(archive)),
    downloaded = as.character(Sys.Date()),
    n_names = nrow(alt)
  )
  saveRDS(provenance, gnrs_provenance_path("geonames", dir))

  gnrs_tidy_archive(dir = dir, keep_archive = keep_archive, quiet = quiet)
  invisible(provenance)
}

#' Read the alternate names for a set of GeoNames identifiers from the archive
#'
#' Internal.  Nineteen million rows, ten tab-separated columns; only the
#' identifier, the language and the name are read, in chunks, and only rows
#' for the wanted identifiers are kept.  The upstream build loads the file with
#' empty fields as NULL and then selects rows whose language is not "link",
#' which in SQL also drops every row with no language at all; the web service
#' confirms that names without a language code do not take part in
#' alternate-name matching, so both are dropped here.  Wikidata identifiers
#' (language "wkdt") are dropped as well: they are identifiers rather than
#' names, and the service does not match on them.
#'
#' @param archive Path to \code{alternateNamesV2.zip}.
#' @param ids Integer GeoNames identifiers to keep.
#' @param quiet Suppress progress messages?
#' @return A data.frame of \code{geonameid} and \code{name}, distinct.
#' @keywords internal
#' @noRd
gnrs_import_altnames <- function(archive, ids, quiet = FALSE) {
  if (!quiet) message("Reading the alternate names (a few minutes) ...")
  con <- unz(archive, "alternateNamesV2.txt", open = "r")
  on.exit(close(con), add = TRUE)

  ids <- as.integer(ids)
  what <- list(NULL, "", "", "", NULL, NULL, NULL, NULL, NULL, NULL)
  kept <- list()
  seen <- 0
  repeat {
    chunk <- scan(
      con, what = what, sep = "\t", quote = "", nmax = 1e6, quiet = TRUE,
      na.strings = character(0), comment.char = "", allowEscapes = FALSE,
      strip.white = FALSE, fill = TRUE, encoding = "UTF-8", multi.line = FALSE
    )
    n <- length(chunk[[2]])
    if (n == 0) {
      break
    }
    seen <- seen + n
    gid <- suppressWarnings(as.integer(chunk[[2]]))
    lang <- chunk[[3]]
    hit <- gid %in% ids & !is.na(lang) & nzchar(lang) & !lang %in% c("link", "wkdt")
    if (any(hit)) {
      kept[[length(kept) + 1]] <- data.frame(
        geonameid = gid[hit], name = chunk[[4]][hit], stringsAsFactors = FALSE
      )
    }
    if (!quiet) message("  ", format(seen, big.mark = ","), " rows read", appendLF = FALSE)
    if (!quiet) message("\r", appendLF = FALSE)
  }
  if (!quiet) message("")

  alt <- if (length(kept) > 0) do.call(rbind, kept) else data.frame(geonameid = integer(0), name = character(0))
  # Invalid byte sequences, should any slip through, are stripped rather than
  # allowed to break later string handling
  alt$name <- iconv(alt$name, "UTF-8", "UTF-8", sub = "")
  alt <- alt[!is.na(alt$name) & nzchar(alt$name), , drop = FALSE]
  alt <- unique(alt)
  alt <- alt[order(alt$geonameid, alt$name), , drop = FALSE]
  rownames(alt) <- NULL
  if (!quiet) message("  ", format(nrow(alt), big.mark = ","), " alternate names kept")
  alt
}

#' Delete the GeoNames archive once its names have been extracted
#'
#' Internal.  Only once the extracted names exist, so an interrupted build
#' never loses the download it would otherwise reuse.  Its size and checksum
#' stay in the provenance record.
#' @keywords internal
#' @noRd
gnrs_tidy_archive <- function(dir = gnrs_cache_dir(), keep_archive = FALSE, quiet = FALSE) {
  if (isTRUE(keep_archive) || !file.exists(gnrs_altnames_path(dir))) {
    return(invisible(FALSE))
  }
  archive <- gnrs_archive_path(dir)
  if (!file.exists(archive)) {
    return(invisible(FALSE))
  }
  freed <- file.size(archive)
  if (unlink(archive) != 0) {
    return(invisible(FALSE))
  }
  record_path <- gnrs_provenance_path("geonames", dir)
  if (file.exists(record_path)) {
    record <- readRDS(record_path)
    record$archive <- NA_character_
    record$archive_kept <- FALSE
    saveRDS(record, record_path)
  }
  if (!quiet) {
    message(
      "  removed the ", round(freed / 1024^2, 1), " MB archive; ",
      "rebuilding 'geonames' would download it again (keep_archive = TRUE to keep it)."
    )
  }
  invisible(TRUE)
}

#' Assemble the name tables the alternate-name steps match against
#'
#' Internal.  R port of the upstream build's name tables: for each level, the
#' GeoNames alternate names of the GeoNames divisions plus their name and
#' ASCII name, all typed "original from geonames"; then, for the GeoNames
#' divisions, any standard name, ASCII name, short name or code not already
#' present for that division, each typed by what it is; then, for every
#' division, its names where the name is absent from the table altogether,
#' typed "from GADM", which is how the divisions added from GADM get their
#' names.  The alternate-name steps match on the GeoNames-typed names; the
#' wildcard steps on all of them.  Absence is judged as the SQL judges it,
#' by the pair of identifier and name for the typed additions and by the name
#' alone for the GADM additions.  Names contributed by a GADM layer built over
#' the snapshot are added last, per division, typed "from GADM" as well.
#'
#' @param dir Cache directory.
#' @param quiet Suppress progress messages?
#' @return The name table, invisibly.
#' @keywords internal
#' @noRd
gnrs_assemble_names <- function(dir = gnrs_cache_dir(), quiet = FALSE) {
  if (!quiet) message("Assembling the name tables ...")
  country <- as.data.frame(nanoparquet::read_parquet(gnrs_reference_path("country", dir)))
  state <- as.data.frame(nanoparquet::read_parquet(gnrs_reference_path("state_province", dir)))
  county <- as.data.frame(nanoparquet::read_parquet(gnrs_reference_path("county_parish", dir)))
  alt <- if (file.exists(gnrs_altnames_path(dir))) {
    as.data.frame(nanoparquet::read_parquet(gnrs_altnames_path(dir)))
  } else {
    data.frame(geonameid = integer(0), name = character(0), stringsAsFactors = FALSE)
  }

  original <- "original from geonames"

  build <- function(tbl, id_col, name_cols, typed, gadm_cols, extra = NULL) {
    ids <- tbl[[id_col]]
    is_geo <- tbl$is_geoname

    rows <- alt[alt$geonameid %in% ids[is_geo], , drop = FALSE]
    out <- data.frame(id = rows$geonameid, name = rows$name, name_type = original, stringsAsFactors = FALSE)
    for (col in name_cols) {
      out <- rbind(out, data.frame(
        id = ids[is_geo], name = tbl[[col]][is_geo], name_type = original, stringsAsFactors = FALSE
      ))
    }
    if (!is.null(extra)) {
      out <- rbind(out, extra)
    }
    out <- out[!is.na(out$name) & nzchar(out$name), , drop = FALSE]
    out <- unique(out[c("id", "name", "name_type")])

    # Typed additions, each only where that division lacks that exact name
    for (i in seq_along(typed)) {
      col <- typed[i]
      type <- names(typed)[i]
      cand <- data.frame(id = ids[is_geo], name = tbl[[col]][is_geo], name_type = type, stringsAsFactors = FALSE)
      cand <- cand[!is.na(cand$name) & nzchar(cand$name), , drop = FALSE]
      have <- paste(out$id, out$name, sep = "\r")
      cand <- cand[!paste(cand$id, cand$name, sep = "\r") %in% have, , drop = FALSE]
      out <- rbind(out, unique(cand))
    }

    # GADM additions, each only where the name is absent from the whole table
    for (col in gadm_cols) {
      cand <- data.frame(id = ids, name = tbl[[col]], name_type = "from GADM", stringsAsFactors = FALSE)
      cand <- cand[!is.na(cand$name) & nzchar(cand$name), , drop = FALSE]
      cand <- cand[!cand$name %in% out$name, , drop = FALSE]
      out <- rbind(out, unique(cand))
    }
    rownames(out) <- NULL
    out
  }

  country_extra <- NULL
  country_names <- build(
    country, "country_id",
    name_cols = "country",
    typed = c(
      "standard name en" = "country", "iso code" = "iso",
      "iso alpha3 code" = "iso_alpha3", "fips code" = "fips"
    ),
    gadm_cols = character(0)
  )
  # The GADM step for countries adds the name and its accent-stripped form
  country$country_unaccent <- gnrs_unaccent(country$country)
  for (col in c("country", "country_unaccent")) {
    cand <- data.frame(id = country$country_id, name = country[[col]], name_type = "from GADM", stringsAsFactors = FALSE)
    cand <- cand[!is.na(cand$name) & nzchar(cand$name) & !cand$name %in% country_names$name, , drop = FALSE]
    country_names <- rbind(country_names, unique(cand))
  }

  extra <- gnrs_extra_state_names()
  extra_ids <- state$state_province_id[match(
    paste(extra$country_iso, extra$state_province_ascii),
    paste(state$country_iso, state$state_province_ascii)
  )]
  found <- !is.na(extra_ids)
  state_extra <- data.frame(
    id = extra_ids[found], name = extra$name[found],
    name_type = rep(original, sum(found)), stringsAsFactors = FALSE
  )
  state_names <- build(
    state, "state_province_id",
    name_cols = c("state_province", "state_province_ascii"),
    # No HASC-typed names: the HASC codes of the deployed database were filled
    # in after this step of the upstream build ran (see gnrs_import_reference)
    typed = c(
      "standard name" = "state_province", "ascii name" = "state_province_ascii",
      "short ascii name" = "state_province_std",
      "short iso code" = "state_province_code", "full iso code" = "state_province_code_full",
      "code2" = "state_province_code2", "full code2" = "state_province_code2_full"
    ),
    gadm_cols = c("state_province", "state_province_ascii", "state_province_std"),
    extra = state_extra
  )

  county_names <- build(
    county, "county_parish_id",
    name_cols = c("county_parish", "county_parish_ascii"),
    typed = c(
      "standard name" = "county_parish", "ascii name" = "county_parish_ascii",
      "short ascii name" = "county_parish_std",
      "short iso code" = "county_parish_code", "full iso code" = "county_parish_code_full",
      "code2" = "county_parish_code2", "full code2" = "county_parish_code2_full"
    ),
    gadm_cols = c("county_parish", "county_parish_ascii", "county_parish_std")
  )

  names <- rbind(
    cbind(level = "country", country_names, stringsAsFactors = FALSE),
    cbind(level = "state_province", state_names, stringsAsFactors = FALSE),
    cbind(level = "county_parish", county_names, stringsAsFactors = FALSE)
  )
  names$id <- as.integer(names$id)

  if (file.exists(gnrs_gadm_names_path(dir))) {
    layer <- as.data.frame(nanoparquet::read_parquet(gnrs_gadm_names_path(dir)))
    layer <- data.frame(
      level = layer$level, id = as.integer(layer$id), name = layer$name,
      name_type = "from GADM", stringsAsFactors = FALSE
    )
    have <- paste(names$level, names$id, names$name, sep = "\r")
    layer <- layer[!paste(layer$level, layer$id, layer$name, sep = "\r") %in% have, , drop = FALSE]
    names <- rbind(names, unique(layer))
  }
  names <- names[order(names$level, names$id, names$name), , drop = FALSE]
  rownames(names) <- NULL

  nanoparquet::write_parquet(names, gnrs_names_path(dir), compression = "gzip")
  if (!quiet) message("  ", format(nrow(names), big.mark = ","), " names")
  invisible(names)
}

#' Make sure the requested components are available locally
#'
#' Internal.  Nothing is downloaded without the user's agreement: either they
#' answer the prompt, or they asked for it in the call by passing
#' \code{build_missing = TRUE}.  That matters because the download is large
#' and lands in the user's cache directory rather than a temporary one.
#'
#' @param sources Components required.
#' @param dir Cache directory.
#' @param build_missing Build what is not present?  Defaults to
#'   \code{interactive()}, which asks first.
#' @param quiet Suppress progress messages?
#' @return TRUE if every requested component is now available, FALSE otherwise.
#' @keywords internal
#' @noRd
gnrs_require_sources <- function(sources, dir = gnrs_cache_dir(),
                                 build_missing = interactive(), quiet = FALSE) {
  present <- vapply(sources, function(s) gnrs_is_built(s, dir), logical(1))
  missing <- sources[!present]
  if (length(missing) == 0) {
    return(TRUE)
  }

  registry <- gnrs_builtin_registry()
  download_mb <- sum(vapply(registry[missing], function(x) x$download_mb, numeric(1)))
  fix <- paste0("GNRS_local_build(", gnrs_source_arg(missing), ")")

  if (!isTRUE(build_missing)) {
    message(
      "No local copy of: ", paste(missing, collapse = ", "), ".",
      "\nRun ", fix, " once to download and prepare it",
      " (about ", download_mb, " MB to download).",
      "\nOr call this function again with build_missing = TRUE to do it now."
    )
    return(FALSE)
  }

  if (interactive()) {
    answer <- readline(paste0(
      "GNRS needs to download ", paste(missing, collapse = ", "),
      ": about ", download_mb, " MB, into\n  ", dir, "\nDownload now? [y/N] "
    ))
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      message("Nothing downloaded. Run ", fix, " when you are ready.")
      return(FALSE)
    }
  }

  GNRS_local_build(sources = missing, dir = dir, quiet = quiet)

  still_missing <- missing[!vapply(missing, function(s) gnrs_is_built(s, dir), logical(1))]
  if (length(still_missing) > 0) {
    message("Could not build: ", paste(still_missing, collapse = ", "), ".")
    return(FALSE)
  }
  TRUE
}
