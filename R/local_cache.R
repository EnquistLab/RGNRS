#' Directory holding the local GNRS reference data
#'
#' The reference data is downloaded on demand rather than shipped with the
#' package, and is kept in the standard user cache directory so that it
#' survives package updates and can be removed with \code{GNRS_local_remove()}.
#'
#' @param create Should the directory be created if it does not exist?
#' @return Path to the cache directory.
#' @keywords internal
#' @noRd
gnrs_cache_dir <- function(create = FALSE) {
  dir <- getOption("GNRS.cache_dir", tools::R_user_dir("GNRS", which = "cache"))
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' Data sources a local build can fetch
#'
#' Internal.  Two components make up the local reference data.
#'
#' \code{"gnrs"} is the web service's own reference tables, fetched through its
#' API: every country, state/province and county/parish it knows, with the
#' identifiers, standard names, codes and GADM identifiers it returns.  Using
#' the service's own tables rather than rebuilding them from GADM and GeoNames
#' means local results carry the same identifiers as results from the web
#' service, and can be joined to them.
#'
#' \code{"geonames"} is the alternate-names file published by GeoNames, from
#' which the service takes the names in other languages, abbreviations and
#' historical names that its alternate-name matching steps use.  The API does
#' not serve these, so they are fetched from the publisher.
#'
#' @return A named list of source definitions.
#' @keywords internal
#' @noRd
gnrs_builtin_registry <- function() {
  list(
    gnrs = list(
      source = "gnrs",
      full_name = "GNRS reference political divisions",
      publisher = "Botanical Information and Ecology Network",
      url = "https://gnrsapi.xyz/gnrs_api.php",
      license = "GPL-3 (software); GADM and GeoNames terms apply to the data",
      citation = paste(
        "Boyle B. L., Maitner B. S., Barbosa G. G. C., Sajja R. K., Feng X.,",
        "Merow C., Newman E. A., Park D. S., Roehrdanz P. R. & Enquist B. J.",
        "(2022). Geographic name resolution service: A tool for the",
        "standardization and indexing of world political division names,",
        "with applications to species distribution modeling. PLOS ONE 17(11):",
        "e0268162. https://doi.org/10.1371/journal.pone.0268162"
      ),
      # Approximate, for the message shown before a download is started: the
      # three API responses together, and the parquet files they become
      download_mb = 25,
      disk_mb = 6
    ),
    geonames = list(
      source = "geonames",
      full_name = "GeoNames alternate names",
      publisher = "GeoNames",
      url = "https://download.geonames.org/export/dump/alternateNamesV2.zip",
      license = "CC BY 4.0",
      citation = "GeoNames (geonames.org). Alternate names. https://www.geonames.org/",
      # The archive is about 195 MB; only the rows for the reference political
      # divisions are kept, which is a few megabytes
      download_mb = 195,
      disk_mb = 8
    )
  )
}

#' Paths of the files a built source consists of
#'
#' Internal.  Everything a source writes is prefixed with its name.  The
#' \code{.gz.parquet} suffix records the codec, so the files are self-describing
#' to anything else that reads them.
#' @keywords internal
#' @noRd
gnrs_reference_path <- function(table, dir = gnrs_cache_dir()) {
  file.path(dir, paste0("gnrs-", table, ".gz.parquet"))
}

#' @keywords internal
#' @noRd
gnrs_altnames_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "geonames-altnames.gz.parquet")
}

#' Path of the assembled name table
#'
#' Internal.  The upstream name tables (\code{country_name},
#' \code{state_province_name}, \code{county_parish_name}) are derived from the
#' reference tables and the GeoNames alternate names together, so they are
#' rebuilt whenever either component is built and stored as one file.
#' @keywords internal
#' @noRd
gnrs_names_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "gnrs-names.gz.parquet")
}

#' @keywords internal
#' @noRd
gnrs_provenance_path <- function(source, dir = gnrs_cache_dir()) {
  file.path(dir, paste0(source, "-provenance.rds"))
}

#' Has a source been built?
#'
#' Internal.  Built means the tables the resolver reads exist, not that a
#' download completed: an interrupted build can leave one without the other.
#' @keywords internal
#' @noRd
gnrs_is_built <- function(source, dir = gnrs_cache_dir()) {
  switch(source,
    gnrs = all(file.exists(gnrs_reference_path(
      c("country", "state_province", "county_parish"), dir
    ))),
    geonames = file.exists(gnrs_altnames_path(dir)),
    FALSE
  )
}

#' Files a source occupies in the cache
#' @keywords internal
#' @noRd
gnrs_source_files <- function(source, dir = gnrs_cache_dir()) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  list.files(dir, pattern = paste0("^", source, "-"), full.names = TRUE)
}

#' How a set of source names would be written in a call
#' @keywords internal
#' @noRd
gnrs_source_arg <- function(sources) {
  quoted <- paste0('"', sources, '"')
  if (length(quoted) == 1) quoted else paste0("c(", paste(quoted, collapse = ", "), ")")
}

#' Report on the locally cached GNRS reference data
#'
#' Shows each component of the local reference data, whether it has been built
#' for offline use, and for those that have, which version it is and how much
#' space it occupies.  A local result can be cited using the versions reported
#' here; \code{GNRS_local_citations()} assembles the citations.
#'
#' Components that have not been built are listed too, with \code{built} FALSE
#' and \code{download_mb} giving what fetching them would cost, so that this is
#' the one place to look to answer both "what did I resolve against" and "what
#' else could I use".  Build them with \code{GNRS_local_build()}.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @return A data.frame with one row per component.  \code{version} and
#'   \code{downloaded} describe what is installed and are NA for a component
#'   that has not been built.  For \code{"gnrs"} the version is the web
#'   service's database version and build date, so a local result can be
#'   compared with the service's results of the same vintage.  \code{size_mb}
#'   is what the component occupies on disk now; \code{download_mb} is what
#'   fetching it costs.
#' @seealso \code{\link{GNRS_local_build}}, \code{\link{GNRS_local_citations}}
#' @export
#' @examples {
#'   status <- GNRS_local_status()
#' }
GNRS_local_status <- function(dir = gnrs_cache_dir()) {
  registry <- gnrs_builtin_registry()
  sources <- names(registry)

  built <- vapply(sources, function(s) gnrs_is_built(s, dir), logical(1))

  provenance <- lapply(sources, function(s) {
    path <- gnrs_provenance_path(s, dir)
    if (file.exists(path)) readRDS(path) else NULL
  })
  names(provenance) <- sources

  from_record <- function(field, empty) {
    vapply(sources, function(s) {
      record <- provenance[[s]]
      if (!built[[s]] || is.null(record)) {
        return(empty)
      }
      value <- record[[field]]
      if (is.null(value) || length(value) == 0) empty else as.character(value)[1]
    }, empty)
  }

  size_mb <- vapply(
    sources,
    function(s) round(sum(file.size(gnrs_source_files(s, dir))) / 1024^2, 1),
    numeric(1)
  )

  out <- data.frame(
    source = sources,
    full_name = vapply(registry, function(x) x$full_name, character(1)),
    built = unname(built),
    version = unname(from_record("version", NA_character_)),
    downloaded = unname(from_record("downloaded", NA_character_)),
    size_mb = unname(size_mb),
    download_mb = vapply(registry, function(x) as.numeric(x$download_mb), numeric(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  absent <- out$source[!out$built]
  if (length(absent) == length(sources)) {
    message(
      "No local reference data built yet in:\n  ", dir,
      "\nRun GNRS_local_build() to set it up."
    )
  } else if (length(absent) > 0) {
    message(
      "Not built: ", paste(absent, collapse = ", "),
      ". Add with GNRS_local_build(", gnrs_source_arg(absent), ")."
    )
  }

  out
}

#' Delete the locally cached GNRS reference data
#'
#' Removes the downloaded reference data and everything derived from it.  The
#' data can be downloaded again at any time with \code{GNRS_local_build()}.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @param ask Ask for confirmation before deleting? Defaults to TRUE in an
#'   interactive session.
#' @return TRUE if anything was removed, FALSE otherwise, invisibly.
#' @export
#' @examples \dontrun{
#' GNRS_local_remove()
#' }
GNRS_local_remove <- function(dir = gnrs_cache_dir(), ask = interactive()) {
  if (!dir.exists(dir)) {
    message("Nothing to remove; no cache directory at:\n  ", dir)
    return(invisible(FALSE))
  }

  size_mb <- round(sum(
    file.size(list.files(dir, recursive = TRUE, full.names = TRUE)),
    na.rm = TRUE
  ) / 1024^2, 1)

  if (ask) {
    answer <- readline(paste0(
      "Delete the local GNRS reference data (", size_mb, " MB) in\n  ", dir,
      "\n? [y/N] "
    ))
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      message("Nothing removed.")
      return(invisible(FALSE))
    }
  }

  unlink(dir, recursive = TRUE)
  gnrs_forget_backbone()
  message("Removed ", size_mb, " MB from ", dir)
  invisible(TRUE)
}

#' Default value for NULL
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
