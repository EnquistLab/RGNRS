#' Citation for the GNRS method itself
#' @keywords internal
#' @noRd
gnrs_method_citation <- function() {
  paste(
    "Boyle B. L., Maitner B. S., Barbosa G. G. C., Sajja R. K., Feng X.,",
    "Merow C., Newman E. A., Park D. S., Roehrdanz P. R. & Enquist B. J.",
    "(2022). Geographic name resolution service: A tool for the",
    "standardization and indexing of world political division names, with",
    "applications to species distribution modeling. PLOS ONE 17(11):",
    "e0268162. https://doi.org/10.1371/journal.pone.0268162"
  )
}

#' Citation for this package
#'
#' Internal.  Uses the package's own citation where it is installed, and falls
#' back to building one from the description, so this works under
#' \code{devtools::load_all()} and in a checkout as well as from a library.
#' @keywords internal
#' @noRd
gnrs_package_citation <- function() {
  built <- suppressWarnings(tryCatch(
    {
      cit <- utils::citation("GNRS")
      trimws(paste(format(cit[1], style = "text"), collapse = " "))
    },
    error = function(e) NULL
  ))
  if (!is.null(built) && nzchar(built)) {
    return(gsub("[[:space:]]+", " ", built))
  }
  version <- suppressWarnings(tryCatch(
    as.character(utils::packageVersion("GNRS")),
    error = function(e) "development version"
  ))
  paste0(
    "Maitner B. & Boyle B. (", format(Sys.Date(), "%Y"), "). GNRS: Access the ",
    "Geographic Name Resolution Service. R package version ", version,
    ". https://github.com/EnquistLab/RGNRS"
  )
}

#' Citations for a local name resolution
#'
#' Assembles everything a result resolved offline should be cited with: the
#' GNRS publication, this package, and the data the names were resolved
#' against, at the version actually built.
#'
#' The reference political divisions are the web service's own tables, which
#' it builds from GADM, GeoNames and Natural Earth; the alternate names come
#' from GeoNames; and where the GADM layer was built, the current GADM release
#' is cited at its version.  A local result is only reproducible if the versions are
#' reported with it, which is why they come from what was built rather than
#' from what is current.  \code{GNRS_local_status()} shows the same versions.
#'
#' @param dir Cache directory. Defaults to the standard user cache location.
#' @param bibtex_file Optional path. If given, the citations are also written
#'   there as BibTeX.
#' @param quiet Suppress the printed citations?
#' @return A data.frame, invisibly, with one row per work to cite:
#'   \code{what} ("method", "software" or "source"), \code{name},
#'   \code{version} and \code{citation}.
#' @seealso \code{\link{GNRS_local_status}} for the versions,
#'   \code{\link{GNRS_citations}} for the web service's own citation list.
#' @export
#' @examples \dontrun{
#' GNRS_local_citations()
#' }
GNRS_local_citations <- function(dir = gnrs_cache_dir(), bibtex_file = NULL, quiet = FALSE) {
  registry <- gnrs_builtin_registry()

  rows <- list(
    data.frame(
      what = "method", name = "GNRS", version = "",
      citation = gnrs_method_citation(), stringsAsFactors = FALSE
    ),
    data.frame(
      what = "software", name = "GNRS R package", version = "",
      citation = gnrs_package_citation(), stringsAsFactors = FALSE
    )
  )

  for (source in names(registry)) {
    if (!gnrs_is_built(source, dir)) {
      next
    }
    path <- gnrs_provenance_path(source, dir)
    record <- if (file.exists(path)) readRDS(path) else list()
    spec <- registry[[source]]
    version <- record$version %||% NA_character_
    accessed <- record$downloaded %||% NA_character_
    citation <- if (source == "gadm") {
      paste0(
        "GADM (", substr(accessed, 1, 4), "). Database of Global Administrative Areas, version ",
        version, ". https://gadm.org/. Accessed ", accessed, "."
      )
    } else if (source == "gnrs") {
      paste0(
        "Boyle B. L., Maitner B., Barbosa G. C. & Enquist B. J. Geographic Name ",
        "Resolution Service reference data, ", version,
        ", built from GADM (https://gadm.org/), GeoNames (https://www.geonames.org/) ",
        "and Natural Earth (https://www.naturalearthdata.com/). Botanical Information ",
        "and Ecology Network, https://gnrs.biendata.org/. Accessed ", accessed, "."
      )
    } else if (source == "points") {
      paste0(
        "GeoNames. Gazetteer (allCountries), file dated ", version,
        ". https://www.geonames.org/ (CC BY 4.0). Accessed ", accessed, "."
      )
    } else {
      paste0(
        "GeoNames. Alternate names (alternateNamesV2), file dated ", version,
        ". https://www.geonames.org/ (CC BY 4.0). Accessed ", accessed, "."
      )
    }
    rows[[length(rows) + 1]] <- data.frame(
      what = "source", name = spec$full_name, version = version,
      citation = citation, stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  if (!quiet) {
    for (i in seq_len(nrow(out))) {
      cat(out$what[i], ": ", out$citation[i], "\n\n", sep = "")
    }
  }

  if (!is.null(bibtex_file)) {
    writeLines(gnrs_citations_bibtex(out), con = bibtex_file)
  }

  invisible(out)
}

#' Render the citation table as BibTeX
#' @keywords internal
#' @noRd
gnrs_citations_bibtex <- function(out) {
  keys <- c(
    method = "boyle2022gnrs", software = "gnrs_r_package"
  )
  entries <- character(0)
  for (i in seq_len(nrow(out))) {
    what <- out$what[i]
    if (what == "method") {
      entries <- c(entries, paste0(
        "@article{", keys[["method"]], ",\n",
        "  author = {Boyle, Bradley L. and Maitner, Brian S. and Barbosa, George G. C. and ",
        "Sajja, Rohith K. and Feng, Xiao and Merow, Cory and Newman, Erica A. and ",
        "Park, Daniel S. and Roehrdanz, Patrick R. and Enquist, Brian J.},\n",
        "  title = {Geographic name resolution service: A tool for the standardization ",
        "and indexing of world political division names, with applications to species ",
        "distribution modeling},\n",
        "  journal = {PLOS ONE},\n  year = {2022},\n  volume = {17},\n  number = {11},\n",
        "  pages = {e0268162},\n  doi = {10.1371/journal.pone.0268162}\n}"
      ))
    } else if (what == "software") {
      entries <- c(entries, paste0(
        "@misc{", keys[["software"]], ",\n",
        "  title = {GNRS: Access the Geographic Name Resolution Service},\n",
        "  author = {Maitner, Brian and Boyle, Brad},\n",
        "  year = {", format(Sys.Date(), "%Y"), "},\n",
        "  note = {R package},\n",
        "  url = {https://github.com/EnquistLab/RGNRS}\n}"
      ))
    } else {
      key <- gsub("[^A-Za-z0-9]", "", paste0(out$name[i], out$version[i]))
      entries <- c(entries, paste0(
        "@misc{", key, ",\n",
        "  title = {", out$name[i], "},\n",
        "  note = {", out$citation[i], "}\n}"
      ))
    }
  }
  paste(entries, collapse = "\n\n")
}
