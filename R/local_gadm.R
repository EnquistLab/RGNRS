#' Fetch the current GADM administrative areas and store their attributes
#'
#' Internal.  GADM's world GeoPackage is one file of about 1.5 GB, holding
#' every administrative area with its names, alternate names, types and HASC
#' and ISO codes.  A GeoPackage is an SQLite database, so the attribute columns
#' can be read with RSQLite without touching the geometry, which is nearly all
#' of the file, and with no spatial software.  Only levels 0 to 2 are kept, as
#' a few megabytes of parquet, and the archive is deleted afterwards unless
#' asked to be kept.
#'
#' @param dir Cache directory.
#' @param overwrite Re-download even if the archive is present?
#' @param keep_archive Keep the downloaded archive after its attributes have
#'   been extracted?
#' @param quiet Suppress progress messages?
#' @return The provenance record, invisibly.
#' @keywords internal
#' @noRd
gnrs_build_gadm <- function(dir, overwrite = FALSE, keep_archive = FALSE, quiet = FALSE) {
  for (pkg in c("DBI", "RSQLite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Reading the GADM GeoPackage needs the '", pkg, "' package. ",
        "Install it with install.packages(\"", pkg, "\") and build again.",
        call. = FALSE
      )
    }
  }
  spec <- gnrs_builtin_registry()$gadm
  archive <- gnrs_gadm_archive_path(dir)

  if (file.exists(archive) && !overwrite) {
    if (!quiet) message("Using cached download of GADM ", spec$version)
  } else {
    if (!quiet) {
      message("Downloading ", spec$full_name, " ", spec$version, " (about ", spec$download_mb, " MB) ...")
    }
    partial <- paste0(archive, ".part")
    old <- options(timeout = max(7200, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    status <- utils::download.file(spec$url, partial, mode = "wb", quiet = quiet, cacheOK = FALSE)
    if (status != 0 || !file.exists(partial)) {
      unlink(partial)
      stop("Download failed for GADM.", call. = FALSE)
    }
    file.rename(partial, archive)
  }

  if (!quiet) message("Extracting the GeoPackage ...")
  members <- utils::unzip(archive, list = TRUE)$Name
  member <- members[grepl("\\.gpkg$", members)][1]
  if (is.na(member)) {
    stop("No GeoPackage found in the GADM archive.", call. = FALSE)
  }
  utils::unzip(archive, files = member, exdir = dir, overwrite = TRUE)
  gpkg <- file.path(dir, member)
  on.exit(unlink(gpkg), add = TRUE)

  if (!quiet) message("Reading the GADM attributes ...")
  gadm <- gnrs_read_gadm_gpkg(gpkg)
  nanoparquet::write_parquet(gadm, gnrs_gadm_path(dir), compression = "gzip")

  provenance <- list(
    source = "gadm",
    full_name = spec$full_name,
    version = spec$version,
    url = spec$url,
    license = spec$license,
    publisher = spec$publisher,
    archive = archive,
    archive_kept = TRUE,
    bytes = as.numeric(file.size(archive)),
    md5 = unname(tools::md5sum(archive)),
    downloaded = as.character(Sys.Date()),
    n_admin0 = sum(gadm$level == 0L),
    n_admin1 = sum(gadm$level == 1L),
    n_admin2 = sum(gadm$level == 2L)
  )
  saveRDS(provenance, gnrs_provenance_path("gadm", dir))
  if (!quiet) {
    message(
      "  ", provenance$n_admin0, " countries, ", provenance$n_admin1,
      " level-1 and ", provenance$n_admin2, " level-2 divisions"
    )
  }

  # With the divisions' coordinates built and sf available, the links are
  # measured against the geometry while it is still on disk: a first pass
  # of the linking, then the distance from each linked division's point to
  # its polygon, which the final pass uses to withdraw links that rest on a
  # code alone and sit too far away
  unlink(gnrs_gadm_distances_path(dir))
  if (gnrs_is_built("points", dir)) {
    if (requireNamespace("sf", quietly = TRUE)) {
      if (!quiet) message("Measuring the links against the GADM geometry ...")
      gnrs_finalize_reference(dir = dir, quiet = TRUE)
      distances <- gnrs_measure_links(gpkg, dir = dir, quiet = quiet)
      nanoparquet::write_parquet(distances, gnrs_gadm_distances_path(dir), compression = "gzip")
      provenance$spatial_check <- TRUE
      provenance$n_measured <- nrow(distances)
      saveRDS(provenance, gnrs_provenance_path("gadm", dir))
    } else if (!quiet) {
      message("  the sf package is not installed, so the links are not checked against the geometry")
    }
  }

  gnrs_tidy_gadm_archive(dir = dir, keep_archive = keep_archive, quiet = quiet)
  invisible(provenance)
}

#' Where the downloaded GADM archive is kept
#' @keywords internal
#' @noRd
gnrs_gadm_archive_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, paste0("gadm-", gnrs_builtin_registry()$gadm$version, ".zip"))
}

#' @keywords internal
#' @noRd
gnrs_gadm_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "gadm-divisions.gz.parquet")
}

#' Delete the downloaded GADM archive once its attributes are stored
#' @keywords internal
#' @noRd
gnrs_tidy_gadm_archive <- function(dir = gnrs_cache_dir(), keep_archive = FALSE, quiet = FALSE) {
  if (isTRUE(keep_archive) || !file.exists(gnrs_gadm_path(dir))) {
    return(invisible(FALSE))
  }
  archive <- gnrs_gadm_archive_path(dir)
  if (!file.exists(archive)) {
    return(invisible(FALSE))
  }
  freed <- file.size(archive)
  if (unlink(archive) != 0) {
    return(invisible(FALSE))
  }
  record_path <- gnrs_provenance_path("gadm", dir)
  if (file.exists(record_path)) {
    record <- readRDS(record_path)
    record$archive <- NA_character_
    record$archive_kept <- FALSE
    saveRDS(record, record_path)
  }
  if (!quiet) {
    message(
      "  removed the ", round(freed / 1024^2, 1), " MB GADM archive; ",
      "rebuilding 'gadm' would download it again (keep_archive = TRUE to keep it)."
    )
  }
  invisible(TRUE)
}

#' Read the levels 0 to 2 of a GADM GeoPackage
#'
#' Internal.  The world GeoPackage has one feature per lowest-level area, with
#' the identifiers and names of every level above it in columns
#' (\code{GID_0}, \code{NAME_0}, \code{GID_1}, \code{NAME_1}, ...).  The
#' distinct combinations at each level are what is wanted, with the names,
#' alternate names, types, HASC and ISO codes and the national statistical
#' code (\code{CC_1}, \code{CC_2}; \code{cc} here).  Read through
#' RSQLite: the geometry column is simply not selected.  GADM writes "NA" for
#' a missing attribute, which becomes NA here.
#'
#' @param path Path to the \code{.gpkg} file.
#' @return A data.frame with one row per division at levels 0, 1 and 2.
#' @keywords internal
#' @noRd
gnrs_read_gadm_gpkg <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tables <- DBI::dbListTables(con)
  table <- tables[!grepl("^(gpkg_|rtree_|sqlite_)", tables)][1]
  if (is.na(table)) {
    stop("No feature table found in the GeoPackage.", call. = FALSE)
  }
  fields <- DBI::dbListFields(con, table)

  wanted <- c(
    "GID_0", "NAME_0", "VARNAME_0",
    "GID_1", "NAME_1", "VARNAME_1", "NL_NAME_1", "TYPE_1", "ENGTYPE_1", "CC_1", "HASC_1", "ISO_1",
    "GID_2", "NAME_2", "VARNAME_2", "NL_NAME_2", "TYPE_2", "ENGTYPE_2", "CC_2", "HASC_2"
  )
  present <- intersect(wanted, fields)
  if (!all(c("GID_0", "NAME_0") %in% present)) {
    stop("The GeoPackage lacks the GID_0 and NAME_0 columns.", call. = FALSE)
  }
  raw <- DBI::dbGetQuery(con, paste0(
    "SELECT DISTINCT ", paste0('"', present, '"', collapse = ", "), ' FROM "', table, '"'
  ))
  col <- function(name) {
    x <- if (name %in% names(raw)) enc2utf8(as.character(raw[[name]])) else rep(NA_character_, nrow(raw))
    x[!is.na(x) & (x == "NA" | !nzchar(x))] <- NA_character_
    x
  }

  level0 <- unique(data.frame(
    level = 0L, gid_0 = col("GID_0"), gid_1 = NA_character_, gid_2 = NA_character_,
    country = col("NAME_0"), name = col("NAME_0"), varname = col("VARNAME_0"),
    nl_name = NA_character_, type = NA_character_, engtype = NA_character_,
    hasc = NA_character_, iso = NA_character_, cc = NA_character_, stringsAsFactors = FALSE
  ))
  level1 <- unique(data.frame(
    level = 1L, gid_0 = col("GID_0"), gid_1 = col("GID_1"), gid_2 = NA_character_,
    country = col("NAME_0"), name = col("NAME_1"), varname = col("VARNAME_1"),
    nl_name = col("NL_NAME_1"), type = col("TYPE_1"), engtype = col("ENGTYPE_1"),
    hasc = col("HASC_1"), iso = col("ISO_1"), cc = col("CC_1"), stringsAsFactors = FALSE
  ))
  level1 <- level1[!is.na(level1$gid_1) & !is.na(level1$name), , drop = FALSE]
  level2 <- unique(data.frame(
    level = 2L, gid_0 = col("GID_0"), gid_1 = col("GID_1"), gid_2 = col("GID_2"),
    country = col("NAME_0"), name = col("NAME_2"), varname = col("VARNAME_2"),
    nl_name = col("NL_NAME_2"), type = col("TYPE_2"), engtype = col("ENGTYPE_2"),
    hasc = col("HASC_2"), iso = NA_character_, cc = col("CC_2"), stringsAsFactors = FALSE
  ))
  level2 <- level2[!is.na(level2$gid_2) & !is.na(level2$name), , drop = FALSE]

  gadm <- rbind(level0, level1, level2)
  gadm <- gadm[!duplicated(gadm[c("level", "gid_0", "gid_1", "gid_2")]), , drop = FALSE]
  gadm <- gadm[order(gadm$level, gadm$gid_0, gadm$gid_1, gadm$gid_2), , drop = FALSE]
  rownames(gadm) <- NULL
  gadm
}

#' Lay the current GADM divisions over the service's reference tables
#'
#' Internal.  R port of the merge the upstream build performs
#' (\code{gadm_geonames_index_*.sql}, \code{geonames_add_gadm_*.sql}), applied
#' to the snapshot with a newer GADM.  Each GADM division is linked to one
#' reference row, or added as a new row when it has none.
#'
#' Countries link by GADM identifier, ISO3 code or name.  States and counties
#' are linked within their parent, each step taking only the divisions the
#' steps before it left unlinked:
#' \enumerate{
#'   \item (counties) the national statistical code GADM carries as
#'     \code{CC_2}, where it is the same code GeoNames carries: judged per
#'     country, the two must agree on at least 95 percent of twenty or more
#'     divisions that link by identifier or name;
#'   \item the GADM identifier the service recorded, version suffix included.
#'     GADM renumbers divisions between releases (every municipality of Brazil
#'     between 3.6 and 4.1), so an identifier that merely looks similar is no
#'     evidence; and it has reused identifiers for other divisions without
#'     changing the suffix (Colombia's COL.27_1 was Santander in 3.6 and is
#'     San Andres in 4.1), so an identifier is believed only when the HASC
#'     codes agree, or when the names agree (the service's own HASC codes are
#'     wrong here and there), or, where either side lacks a HASC code, when
#'     the names are at least somewhat alike under the same parent; and not
#'     even then when the names do not agree exactly (one merely containing
#'     the other does not count here) and another GADM division bears the
#'     reference name, or one of its alternate names, exactly, or a different
#'     GeoNames division bears the GADM name exactly (the service's
#'     identifiers and HASC codes for Guyana's regions are consistently
#'     shifted by one, its identifier for many a Russian rayon is the
#'     identifier of the town of the same root, its identifier for Cape
#'     Verde's Santa Catarina is that of Santa Catarina do Fogo, and its
#'     identifier for Cidade de Maputo is that of Maputo province);
#'   \item the HASC code, withheld on the same terms when the names disagree
#'     and another GADM division bears the name (the service's HASC codes
#'     are shifted along with its identifiers where they are shifted);
#'   \item the name, accent-stripped and case-folded, against the reference
#'     name and its standardised form; GADM writes a qualifier in brackets
#'     ("Osh (city)", "Vientiane [prefecture]"), so the name with the
#'     brackets dropped is tried as well, and so are the words in either
#'     order ("Cao Lanh (Thanh pho)" and "Thanh Pho Cao Lanh");
#'   \item the name against the GeoNames alternate names of the reference
#'     divisions, where the alternate names have been built ("Mwali" is an
#'     alternate name of Moheli, "Oromia" of Oromiya), where the alternate
#'     name belongs to one division under the parent and the GADM name to
#'     one GADM division;
#'   \item the name reduced to its distinguishing words on both sides, type
#'     words and particles removed ("Gemeente Zutphen" and "Zutphen",
#'     "Shahrestan-e Fasa" and "Fasa"), where the reduced name is unique on
#'     each side;
#'   \item a GADM name cut off at 32 characters (a limit of its older
#'     shapefiles that the GeoPackage inherits) against the beginnings of the
#'     names under the parent, where exactly one begins so;
#'   \item one name being whole words of the other ("Bregenz" and
#'     "Politischer Bezirk Bregenz", "Cucuta" and "San Jose de Cucuta"),
#'     between a GADM division and a GeoNames division that nothing else has
#'     linked, where the pairing is the only one possible under the parent and
#'     the extra words are not qualifiers such as North, Urban or City, which
#'     tell two divisions apart rather than one division's names.
#' }
#' The upstream build's wildcard pass is the last of these without its
#' conditions (any substring, any candidate); left free it links "Reading" to
#' "North Reading" and "Kadoma" to "Kadoma Urban".
#'
#' When the distances from the divisions' GeoNames points to the polygons
#' they were linked to are known (\code{distances}, measured by
#' \code{gnrs_measure_links()} while the GeoPackage was on disk), a link
#' that does not rest on the names agreeing outright is withdrawn when its
#' point lies more than \code{max_km} from the polygon: GeoNames' points sit
#' near an edge often, but not that far off.  The GADM division is then
#' added as a new one and the reference row keeps no identifier.
#'
#' The service's tables hold about a thousand divisions twice, the GADM name
#' its build added beside the GeoNames row, and the added row is the one that
#' carries a HASC code.  Such a row stands for its GeoNames row here (the two
#' having the same name under the same parent, or the same reduced name, or
#' the added name being a whole word or words of the GeoNames name, as
#' "Braunau am Inn" is of "Politischer Bezirk Braunau am Inn"): whatever
#' would link to it links to the GeoNames row instead, and it is left without
#' identifiers.  Some of the service's copies sit under the wrong parent (the
#' cantons of Napo under Tungurahua, whose codes the service also has the
#' wrong way round); an identifier or HASC code that points at such a copy is
#' redirected to the GeoNames row bearing the GADM name under the GADM
#' division's own parent.  Among rows that are not duplicates the GeoNames row is
#' preferred, then the lowest identifier, and no reference row is linked
#' twice.  A linked row
#' takes the current GADM identifiers, and its HASC code where it had none.  A
#' GADM division with no counterpart is added as a new row, numbered above
#' every existing identifier, as the upstream build numbers its additions.  A
#' reference row with no counterpart in this GADM keeps its place but loses
#' its GADM identifiers, so that the \code{gid_*} columns always refer to the
#' version that was built.
#'
#' @param country,state,county The snapshot tables.
#' @param gadm The GADM attribute table from \code{gnrs_read_gadm_gpkg()}.
#' @param altnames The GeoNames alternate names (columns \code{geonameid},
#'   \code{name}), or NULL when the alternate names are not built.
#' @param distances Distances from each reference division's point to the
#'   GADM polygon it was linked to on an earlier pass (columns
#'   \code{geonameid}, \code{level}, \code{gid}, \code{km}), or NULL.
#' @param max_km Beyond this distance a link that the names do not settle is
#'   withdrawn.
#' @param code_min_n,code_min_agree How many divisions of a country must link
#'   by identifier or name and carry a statistical code on both sides, and
#'   what share of them must agree, before the code is trusted for that
#'   country.
#' @return A list of the three updated tables, the names GADM contributes
#'   (\code{names}: level, id, name), the link counts and the counts per
#'   linking step.
#' @keywords internal
#' @noRd
gnrs_apply_gadm <- function(country, state, county, gadm, altnames = NULL, distances = NULL, max_km = 25, code_min_n = 20, code_min_agree = 0.95) {
  next_id <- max(c(country$country_id, state$state_province_id, county$county_parish_id), na.rm = TRUE) + 1L
  fold <- function(x) gnrs_lower(gnrs_unaccent(x))
  key <- gnrs_key
  n_split <- function(x) lengths(strsplit(ifelse(is.na(x), "", x), "|", fixed = TRUE))
  split_names <- function(x) trimws(unlist(strsplit(ifelse(is.na(x), "", x), "|", fixed = TRUE), use.names = FALSE))
  if (!"cc" %in% names(gadm)) gadm$cc <- NA_character_
  counts <- list()
  steps <- list()
  names_out <- list()

  # The service's tables hold the same division more than once where its
  # build added the GADM name beside the GeoNames row.  match() takes the
  # first hit, so the GeoNames row goes first, then the lowest identifier.
  state <- state[order(!(state$is_geoname %in% TRUE), state$state_province_id), , drop = FALSE]
  county <- county[order(!(county$is_geoname %in% TRUE), county$county_parish_id), , drop = FALSE]
  rownames(state) <- NULL
  rownames(county) <- NULL

  # --- Countries ------------------------------------------------------------
  g0 <- gadm[gadm$level == 0L, , drop = FALSE]
  # GADM's own exports have written some country names without their spaces
  g0_name <- gnrs_gadm_country_name(g0$name)
  link0 <- match(g0$gid_0, country$gid_0)
  miss <- is.na(link0)
  link0[miss] <- match(g0$gid_0[miss], country$iso_alpha3)
  # A code that changed between releases (Kosovo, Northern Cyprus) is caught
  # by the name
  miss <- is.na(link0)
  link0[miss] <- match(fold(g0_name[miss]), fold(country$country))
  link0 <- gnrs_link_fill(rep(NA_integer_, length(link0)), link0)

  new0 <- which(is.na(link0))
  if (length(new0) > 0) {
    ids <- next_id + seq_along(new0) - 1L
    next_id <- next_id + length(new0)
    added <- data.frame(
      country_id = ids, country = g0_name[new0], iso = NA_character_,
      iso_alpha3 = g0$gid_0[new0], fips = NA_character_, continent_code = NA_character_,
      continent = NA_character_, gid_0 = g0$gid_0[new0], is_geoname = FALSE,
      alt_country_id = NA_integer_, stringsAsFactors = FALSE
    )
    country <- rbind(country[names(added)], added)
    link0[new0] <- match(ids, country$country_id)
  }
  country$gid_0 <- NA_character_
  country$gid_0[link0] <- g0$gid_0
  counts$country_linked <- sum(!is.na(link0)) - length(new0)
  counts$country_added <- length(new0)
  names_out$country <- data.frame(
    id = country$country_id[link0], name = c(g0_name, gnrs_unaccent(g0_name)),
    stringsAsFactors = FALSE
  )
  gadm_country_id <- stats::setNames(country$country_id[link0], g0$gid_0)

  # --- States / provinces -----------------------------------------------------
  g1 <- gadm[gadm$level == 1L, , drop = FALSE]
  g1$country_id <- unname(gadm_country_id[g1$gid_0])
  g1 <- g1[!is.na(g1$country_id), , drop = FALSE]
  g1$fold <- fold(g1$name)
  g1$fold2 <- gnrs_unbracket(g1$fold)
  g1$sorted <- gnrs_sort_words(ifelse(is.na(g1$fold2), g1$fold, g1$fold2))
  words1 <- gnrs_type_words(g1, g1$country_id)
  g1$reduced <- gnrs_unique_key(key(g1$country_id, gnrs_reduce_name(g1$name, g1$country_id, words1)))

  st_ascii <- fold(state$state_province_ascii)
  st_std <- fold(state$state_province_std)
  st_reduced <- list(
    gnrs_unique_key(key(state$country_id, gnrs_reduce_name(state$state_province_ascii, state$country_id, words1))),
    gnrs_unique_key(key(state$country_id, gnrs_reduce_name(state$state_province_std, state$country_id, words1)))
  )
  st_sorted <- list(gnrs_sort_words(st_ascii), gnrs_sort_words(st_std))
  st_alias <- gnrs_alias(state$country_id, st_ascii, st_std, st_reduced[[1]], state$is_geoname)
  st_alt <- gnrs_alt_keys(altnames, state$state_province_id, state$country_id, st_alias)
  g1$alt_hit <- gnrs_alt_match(key(g1$country_id, g1$fold), st_alt)
  g1$alt_hit <- ifelse(is.na(g1$alt_hit), gnrs_alt_match(key(g1$country_id, g1$fold2), st_alt), g1$alt_hit)

  named_by1 <- gnrs_named_by(key(state$country_id, st_ascii), key(state$country_id, st_std), st_reduced[[1]], key(g1$country_id, g1$fold), g1$reduced, g1$alt_hit, key(g1$country_id, g1$fold2))
  geo_named1 <- gnrs_geoname_named(key(state$country_id, st_ascii), key(state$country_id, st_std), st_reduced[[1]], state$is_geoname, key(g1$country_id, g1$fold), g1$reduced, g1$alt_hit, key(g1$country_id, g1$fold2))
  st_sorted_hit <- gnrs_alt_match(gnrs_unique_key(key(g1$country_id, g1$sorted)), data.frame(key = c(gnrs_unique_key(key(state$country_id, st_sorted[[1]])), gnrs_unique_key(key(state$country_id, st_sorted[[2]]))), row = rep(seq_len(nrow(state)), 2), stringsAsFactors = FALSE))
  g1$alt_hit <- ifelse(is.na(g1$alt_hit), st_sorted_hit, g1$alt_hit)
  uncontested1 <- function(candidate) {
    candidate <- gnrs_prefer_geoname(candidate, state$is_geoname, geo_named1)
    gnrs_uncontested(candidate, g1$fold, g1$reduced, g1$alt_hit, st_ascii, st_std, st_reduced[[1]], named_by1, g1$fold2, geo_named1)
  }
  by_gid <- gnrs_prefer_geoname(st_alias[match(g1$gid_1, state$gid_1)], state$is_geoname, geo_named1)
  by_gid <- gnrs_believe_identifier(
    by_gid, g1$hasc, state$hasc_full, g1$fold, g1$reduced, g1$alt_hit, st_ascii, st_std, st_reduced[[1]],
    same_parent = state$country_id[by_gid] == g1$country_id,
    named_by = named_by1, fold_g2 = g1$fold2, claimant = geo_named1
  )
  link1 <- gnrs_link_fill(rep(NA_integer_, nrow(g1)), by_gid)
  steps1 <- gnrs_step_count(integer(0), "identifier", link1)
  link1 <- gnrs_link_step(link1, key(g1$country_id, g1$hasc), key(state$country_id, state$hasc_full), st_alias, uncontested1)
  steps1 <- gnrs_step_count(steps1, "hasc", link1)
  link1 <- gnrs_link_step(link1, key(g1$country_id, g1$fold), list(key(state$country_id, st_ascii), key(state$country_id, st_std)), st_alias)
  link1 <- gnrs_link_step(link1, key(g1$country_id, g1$fold2), list(key(state$country_id, st_ascii), key(state$country_id, st_std)), st_alias)
  link1 <- gnrs_link_step(link1, gnrs_unique_key(key(g1$country_id, g1$sorted)), list(gnrs_unique_key(key(state$country_id, st_sorted[[1]])), gnrs_unique_key(key(state$country_id, st_sorted[[2]]))), st_alias)
  steps1 <- gnrs_step_count(steps1, "name", link1)
  link1 <- gnrs_link_fill(link1, ifelse(is.na(link1), g1$alt_hit, NA_integer_))
  steps1 <- gnrs_step_count(steps1, "alternate_name", link1)
  link1 <- gnrs_link_step(link1, g1$reduced, st_reduced, st_alias)
  steps1 <- gnrs_step_count(steps1, "reduced_name", link1)
  link1 <- gnrs_link_truncated(link1, g1$name, g1$fold, g1$country_id, state$country_id, list(st_ascii, st_std), state$is_geoname, st_alias)
  steps1 <- gnrs_step_count(steps1, "truncated_name", link1)
  link1 <- gnrs_link_contained(link1, g1$fold, g1$country_id, state$country_id, st_ascii, st_std, state$is_geoname, st_alias)
  steps1 <- gnrs_step_count(steps1, "contained_name", link1)
  link1 <- gnrs_spatial_veto(link1, state$state_province_id, g1$gid_1, gnrs_exact_link(link1, g1$fold, g1$fold2, g1$reduced, g1$alt_hit, st_ascii, st_std, st_reduced[[1]]), distances, "state_province", max_km)
  steps1["spatial_veto"] <- sum(steps1) - sum(!is.na(link1))

  new1 <- which(is.na(link1))
  if (length(new1) > 0) {
    ids <- next_id + seq_along(new1) - 1L
    next_id <- next_id + length(new1)
    parent <- match(g1$country_id[new1], country$country_id)
    ascii <- gnrs_unaccent(g1$name[new1])
    added <- data.frame(
      state_province_id = ids, country_id = g1$country_id[new1],
      country_iso = country$iso[parent], country = country$country[parent],
      state_province = g1$name[new1], state_province_ascii = ascii,
      state_province_std = ascii, state_province_code_full = NA_character_,
      hasc_full = g1$hasc[new1], state_province_code2_full = NA_character_,
      gid_0 = g1$gid_0[new1], gid_1 = g1$gid_1[new1], is_geoname = FALSE,
      state_province_code = NA_character_, hasc = NA_character_,
      state_province_code2 = NA_character_,
      is_countryasstate = g1$name[new1] %in% gnrs_country_as_state_names(),
      stringsAsFactors = FALSE
    )
    state <- rbind(state[names(added)], added)
    link1[new1] <- match(ids, state$state_province_id)
  }
  state$gid_0 <- NA_character_
  state$gid_1 <- NA_character_
  state$gid_0[link1] <- g1$gid_0
  state$gid_1[link1] <- g1$gid_1
  fill <- is.na(state$hasc_full[link1]) & !is.na(g1$hasc)
  state$hasc_full[link1[fill]] <- g1$hasc[fill]
  counts$state_linked <- sum(!is.na(link1)) - length(new1)
  counts$state_added <- length(new1)
  steps$state <- steps1
  names_out$state <- data.frame(
    id = c(
      state$state_province_id[link1], state$state_province_id[link1],
      rep(state$state_province_id[link1], n_split(g1$varname)),
      rep(state$state_province_id[link1], n_split(g1$nl_name))
    ),
    name = c(g1$name, gnrs_unaccent(g1$name), split_names(g1$varname), split_names(g1$nl_name)),
    stringsAsFactors = FALSE
  )
  gadm_state_id <- stats::setNames(state$state_province_id[link1], g1$gid_1)

  # --- Counties / parishes ----------------------------------------------------
  g2 <- gadm[gadm$level == 2L, , drop = FALSE]
  g2$state_province_id <- unname(gadm_state_id[g2$gid_1])
  g2 <- g2[!is.na(g2$state_province_id), , drop = FALSE]
  g2$country_id <- state$country_id[match(g2$state_province_id, state$state_province_id)]
  g2$fold <- fold(g2$name)
  g2$fold2 <- gnrs_unbracket(g2$fold)
  g2$sorted <- gnrs_sort_words(ifelse(is.na(g2$fold2), g2$fold, g2$fold2))
  words2 <- gnrs_type_words(g2, g2$country_id)
  g2$reduced <- gnrs_unique_key(key(g2$state_province_id, gnrs_reduce_name(g2$name, g2$country_id, words2)))
  ct_ascii <- fold(county$county_parish_ascii)
  ct_std <- fold(county$county_parish_std)
  ct_reduced <- gnrs_unique_key(key(county$state_province_id, gnrs_reduce_name(county$county_parish_ascii, county$country_id, words2)))
  ct_reduced_std <- gnrs_unique_key(key(county$state_province_id, gnrs_reduce_name(county$county_parish_std, county$country_id, words2)))
  ct_sorted <- list(gnrs_sort_words(ct_ascii), gnrs_sort_words(ct_std))
  ct_alias <- gnrs_alias(county$state_province_id, ct_ascii, ct_std, ct_reduced, county$is_geoname)
  ct_alt <- gnrs_alt_keys(altnames, county$county_parish_id, county$state_province_id, ct_alias)
  g2$alt_hit <- gnrs_alt_match(key(g2$state_province_id, g2$fold), ct_alt)
  g2$alt_hit <- ifelse(is.na(g2$alt_hit), gnrs_alt_match(key(g2$state_province_id, g2$fold2), ct_alt), g2$alt_hit)

  # The recorded identifier, believed on the terms above
  named_by2 <- gnrs_named_by(key(county$state_province_id, ct_ascii), key(county$state_province_id, ct_std), ct_reduced, key(g2$state_province_id, g2$fold), g2$reduced, g2$alt_hit, key(g2$state_province_id, g2$fold2))
  geo_named2 <- gnrs_geoname_named(key(county$state_province_id, ct_ascii), key(county$state_province_id, ct_std), ct_reduced, county$is_geoname, key(g2$state_province_id, g2$fold), g2$reduced, g2$alt_hit, key(g2$state_province_id, g2$fold2))
  ct_sorted_hit <- gnrs_alt_match(gnrs_unique_key(key(g2$state_province_id, g2$sorted)), data.frame(key = c(gnrs_unique_key(key(county$state_province_id, ct_sorted[[1]])), gnrs_unique_key(key(county$state_province_id, ct_sorted[[2]]))), row = rep(seq_len(nrow(county)), 2), stringsAsFactors = FALSE))
  g2$alt_hit <- ifelse(is.na(g2$alt_hit), ct_sorted_hit, g2$alt_hit)
  uncontested2 <- function(candidate) {
    candidate <- gnrs_prefer_geoname(candidate, county$is_geoname, geo_named2)
    gnrs_uncontested(candidate, g2$fold, g2$reduced, g2$alt_hit, ct_ascii, ct_std, ct_reduced, named_by2, g2$fold2, geo_named2)
  }
  by_gid <- gnrs_prefer_geoname(ct_alias[match(g2$gid_2, county$gid_2)], county$is_geoname, geo_named2)
  by_gid <- gnrs_believe_identifier(
    by_gid, g2$hasc, county$hasc_2_full, g2$fold, g2$reduced, g2$alt_hit, ct_ascii, ct_std, ct_reduced,
    same_parent = county$state_province_id[by_gid] == g2$state_province_id,
    named_by = named_by2, fold_g2 = g2$fold2, claimant = geo_named2
  )

  # The statistical code, where GADM's and GeoNames' agree for the country
  ct_code <- gnrs_last_segment(county$county_parish_code_full)
  by_code <- ct_alias[match(key(g2$state_province_id, g2$cc), key(county$state_province_id, ct_code))]
  by_code[is.na(g2$cc)] <- NA_integer_
  provisional <- gnrs_link_fill(rep(NA_integer_, nrow(g2)), by_gid)
  provisional <- gnrs_link_step(provisional, key(g2$state_province_id, g2$fold), list(key(county$state_province_id, ct_ascii), key(county$state_province_id, ct_std)), ct_alias)
  both <- !is.na(by_code) & !is.na(provisional)
  agreement <- split(by_code[both] == provisional[both], g2$country_id[both])
  trusted <- names(agreement)[vapply(agreement, function(a) length(a) >= code_min_n && mean(a) >= code_min_agree, logical(1))]
  by_code[!(as.character(g2$country_id) %in% trusted)] <- NA_integer_

  link2 <- gnrs_link_fill(rep(NA_integer_, nrow(g2)), by_code)
  steps2 <- gnrs_step_count(integer(0), "statistical_code", link2)
  link2 <- gnrs_link_fill(link2, by_gid)
  steps2 <- gnrs_step_count(steps2, "identifier", link2)
  link2 <- gnrs_link_step(link2, key(g2$state_province_id, g2$hasc), key(county$state_province_id, county$hasc_2_full), ct_alias, uncontested2)
  steps2 <- gnrs_step_count(steps2, "hasc", link2)
  link2 <- gnrs_link_step(link2, key(g2$state_province_id, g2$fold), list(key(county$state_province_id, ct_ascii), key(county$state_province_id, ct_std)), ct_alias)
  link2 <- gnrs_link_step(link2, key(g2$state_province_id, g2$fold2), list(key(county$state_province_id, ct_ascii), key(county$state_province_id, ct_std)), ct_alias)
  link2 <- gnrs_link_step(link2, gnrs_unique_key(key(g2$state_province_id, g2$sorted)), list(gnrs_unique_key(key(county$state_province_id, ct_sorted[[1]])), gnrs_unique_key(key(county$state_province_id, ct_sorted[[2]]))), ct_alias)
  steps2 <- gnrs_step_count(steps2, "name", link2)
  link2 <- gnrs_link_fill(link2, ifelse(is.na(link2), g2$alt_hit, NA_integer_))
  steps2 <- gnrs_step_count(steps2, "alternate_name", link2)
  link2 <- gnrs_link_step(link2, g2$reduced, list(ct_reduced, ct_reduced_std), ct_alias)
  steps2 <- gnrs_step_count(steps2, "reduced_name", link2)
  link2 <- gnrs_link_truncated(link2, g2$name, g2$fold, g2$state_province_id, county$state_province_id, list(ct_ascii, ct_std), county$is_geoname, ct_alias)
  steps2 <- gnrs_step_count(steps2, "truncated_name", link2)
  link2 <- gnrs_link_contained(link2, g2$fold, g2$state_province_id, county$state_province_id, ct_ascii, ct_std, county$is_geoname, ct_alias)
  steps2 <- gnrs_step_count(steps2, "contained_name", link2)
  link2 <- gnrs_spatial_veto(link2, county$county_parish_id, g2$gid_2, gnrs_exact_link(link2, g2$fold, g2$fold2, g2$reduced, g2$alt_hit, ct_ascii, ct_std, ct_reduced), distances, "county_parish", max_km)
  steps2["spatial_veto"] <- sum(steps2) - sum(!is.na(link2))

  new2 <- which(is.na(link2))
  if (length(new2) > 0) {
    ids <- next_id + seq_along(new2) - 1L
    next_id <- next_id + length(new2)
    parent <- match(g2$state_province_id[new2], state$state_province_id)
    ascii <- gnrs_unaccent(g2$name[new2])
    added <- data.frame(
      county_parish_id = ids, country_id = state$country_id[parent],
      country = state$country[parent], country_iso = state$country_iso[parent],
      state_province_id = g2$state_province_id[new2],
      state_province_ascii = state$state_province_ascii[parent],
      county_parish = g2$name[new2], county_parish_ascii = ascii,
      county_parish_std = ascii, county_parish_code_full = NA_character_,
      county_parish_code2_full = NA_character_, hasc_2_full = g2$hasc[new2],
      gid_0 = g2$gid_0[new2], gid_1 = g2$gid_1[new2], gid_2 = g2$gid_2[new2],
      is_geoname = FALSE, county_parish_code = NA_character_,
      hasc_2 = NA_character_, county_parish_code2 = NA_character_,
      is_stateascounty = state$state_province_ascii[parent] %in% gnrs_country_as_state_names(),
      stringsAsFactors = FALSE
    )
    county <- rbind(county[names(added)], added)
    link2[new2] <- match(ids, county$county_parish_id)
  }
  county$gid_0 <- NA_character_
  county$gid_1 <- NA_character_
  county$gid_2 <- NA_character_
  county$gid_0[link2] <- g2$gid_0
  county$gid_1[link2] <- g2$gid_1
  county$gid_2[link2] <- g2$gid_2
  fill <- is.na(county$hasc_2_full[link2]) & !is.na(g2$hasc)
  county$hasc_2_full[link2[fill]] <- g2$hasc[fill]
  counts$county_linked <- sum(!is.na(link2)) - length(new2)
  counts$county_added <- length(new2)
  steps$county <- steps2
  steps$county_code_countries <- trusted
  names_out$county <- data.frame(
    id = c(
      county$county_parish_id[link2], county$county_parish_id[link2],
      rep(county$county_parish_id[link2], n_split(g2$varname)),
      rep(county$county_parish_id[link2], n_split(g2$nl_name))
    ),
    name = c(g2$name, gnrs_unaccent(g2$name), split_names(g2$varname), split_names(g2$nl_name)),
    stringsAsFactors = FALSE
  )

  names <- rbind(
    cbind(level = "country", names_out$country, stringsAsFactors = FALSE),
    cbind(level = "state_province", names_out$state, stringsAsFactors = FALSE),
    cbind(level = "county_parish", names_out$county, stringsAsFactors = FALSE)
  )
  names <- names[!is.na(names$id) & !is.na(names$name) & nzchar(names$name), , drop = FALSE]
  names <- unique(names)
  rownames(names) <- NULL

  list(
    country = country[order(country$country_id), , drop = FALSE],
    state = state[order(state$state_province_id), , drop = FALSE],
    county = county[order(county$county_parish_id), , drop = FALSE],
    names = names,
    counts = counts,
    steps = steps
  )
}

#' Link unlinked GADM rows to candidate reference rows, each row once
#'
#' Internal.  \code{link} holds, for each GADM row, the index of the reference
#' row it is linked to, or NA.  Rows still NA take their candidate unless the
#' candidate is already linked to another GADM row; two rows with the same
#' candidate take it in order, the first winning.
#' @keywords internal
#' @noRd
gnrs_link_fill <- function(link, candidate, alias = NULL) {
  if (!is.null(alias)) candidate <- alias[candidate]
  taken <- unique(link[!is.na(link)])
  miss <- is.na(link) & !is.na(candidate) & !(candidate %in% taken)
  cand <- candidate[miss]
  cand[duplicated(cand)] <- NA_integer_
  link[miss] <- cand
  link
}

#' One linking step on keys
#'
#' Internal.  Matches the keys of the GADM rows still unlinked against the
#' keys of the reference rows not yet linked.  Several reference keys may be
#' given (the name and its standardised form): a GADM row takes the earliest
#' reference row any of them hits, so the preferred row wins whichever form
#' it matches by.  A missing key on either side never matches:
#' \code{match()} would otherwise pair every division without a HASC code
#' with the first reference row without one.
#' @keywords internal
#' @noRd
gnrs_link_step <- function(link, key_g, key_ref, alias = NULL, filter = NULL) {
  if (!is.list(key_ref)) key_ref <- list(key_ref)
  taken <- unique(link[!is.na(link)])
  if (!is.null(alias)) taken <- which(alias %in% taken)
  candidate <- rep(NA_integer_, length(key_g))
  for (k in key_ref) {
    k[taken] <- NA_character_
    m <- match(key_g, k)
    candidate <- pmin(candidate, m, na.rm = TRUE)
  }
  candidate[is.na(key_g)] <- NA_integer_
  if (!is.null(alias)) candidate <- alias[candidate]
  if (!is.null(filter)) candidate <- filter(candidate)
  gnrs_link_fill(link, candidate)
}

#' Which reference row each duplicate row stands for
#'
#' Internal.  A row that is not from GeoNames is a duplicate of a GeoNames
#' row under the same parent when the two have the same folded name (by name
#' or standardised name, either way round), or the same reduced name, or when
#' its name is a whole word or words of the GeoNames row's name and no other
#' GeoNames row's under that parent.  Returns, for every row, the index of
#' the row it stands for: itself for all but the duplicates.
#' @keywords internal
#' @noRd
gnrs_alias <- function(parent, ascii, std, reduced, is_geoname) {
  k_ascii <- gnrs_key(parent, ascii)
  k_std <- gnrs_key(parent, std)
  safe_match <- function(x, table) {
    m <- match(x, table)
    m[is.na(x)] <- NA_integer_
    m
  }
  geo <- which(is_geoname %in% TRUE)
  dup <- which(!(is_geoname %in% TRUE))
  alias <- seq_along(parent)
  if (length(geo) == 0 || length(dup) == 0) return(alias)
  hit <- pmin(
    safe_match(k_ascii[dup], k_ascii[geo]), safe_match(k_ascii[dup], k_std[geo]),
    safe_match(k_std[dup], k_std[geo]), safe_match(k_std[dup], k_ascii[geo]),
    safe_match(reduced[dup], reduced[geo]),
    na.rm = TRUE
  )
  alias[dup[!is.na(hit)]] <- geo[hit[!is.na(hit)]]

  # Whole-word containment, one parent at a time
  left <- dup[is.na(hit) & !is.na(ascii[dup]) & nchar(ascii[dup]) > 3]
  geo_by_parent <- split(geo, parent[geo])
  for (i in left) {
    cand <- geo_by_parent[[as.character(parent[i])]]
    if (length(cand) == 0) next
    needle <- paste0(" ", ascii[i], " ")
    within <- grepl(needle, paste0(" ", ascii[cand], " "), fixed = TRUE) |
      grepl(needle, paste0(" ", std[cand], " "), fixed = TRUE)
    if (sum(within) == 1) alias[i] <- cand[within]
  }
  alias
}

#' Link GADM names cut off at 32 characters
#'
#' Internal.  Older GADM releases stored names in shapefiles, which hold 32
#' characters, and the GeoPackage keeps the cut-off names ("Heroica Ciudad de
#' Huajuapan de L").  Such a name links to the one reference row under its
#' parent whose name begins with it.
#' @keywords internal
#' @noRd
gnrs_link_truncated <- function(link, name, fold_g, parent_g, parent_ref, folds_ref, is_geoname, alias) {
  cut <- which(is.na(link) & !is.na(name) & nchar(name) == 32L)
  if (length(cut) == 0) return(link)
  taken <- unique(link[!is.na(link)])
  taken <- which(alias %in% taken)
  rows <- seq_along(alias)
  rows_by_parent <- split(rows, parent_ref[rows])
  candidate <- rep(NA_integer_, length(link))
  for (i in cut) {
    cand <- rows_by_parent[[as.character(parent_g[i])]]
    cand <- setdiff(cand, taken)
    if (length(cand) == 0) next
    begins <- Reduce(`|`, lapply(folds_ref, function(f) startsWith(ifelse(is.na(f[cand]), "", f[cand]), fold_g[i])))
    hit <- unique(alias[cand[begins]])
    if (length(hit) == 1) candidate[i] <- hit
  }
  gnrs_link_fill(link, candidate, alias)
}

#' Link a name that is whole words of another
#'
#' Internal.  The last step, for GADM divisions nothing else has linked,
#' against the GeoNames divisions under the same parent that nothing has
#' linked either: one folded name must be a whole word or words of the other,
#' the pairing must be the only one possible either way, and the words the
#' longer name adds must not be qualifiers that distinguish divisions from one
#' another (North, Upper, Urban, City...).  "Bregenz" links "Politischer
#' Bezirk Bregenz" and "Cucuta" links "San Jose de Cucuta"; "Lincolnshire"
#' does not link "North East Lincolnshire".
#' @keywords internal
#' @noRd
gnrs_link_contained <- function(link, fold_g, parent_g, parent_ref, ascii_ref, std_ref, is_geoname, alias) {
  todo <- which(is.na(link) & !is.na(fold_g) & nchar(fold_g) > 3)
  if (length(todo) == 0) return(link)
  taken <- unique(link[!is.na(link)])
  taken <- which(alias %in% taken)
  free <- setdiff(which(is_geoname %in% TRUE), taken)
  free_by_parent <- split(free, parent_ref[free])
  qualifiers <- gnrs_qualifier_words()
  words <- function(x) strsplit(x, "[^\\p{L}\\p{N}]+", perl = TRUE)
  contained <- function(short, long) {
    ok <- !is.na(long) & nchar(long) > nchar(short) & grepl(paste0(" ", short, " "), paste0(" ", long, " "), fixed = TRUE)
    if (!any(ok)) return(ok)
    extra <- lapply(words(long[ok]), setdiff, y = words(short)[[1]])
    ok[ok] <- !vapply(extra, function(w) any(w %in% qualifiers), logical(1))
    ok
  }
  candidate <- rep(NA_integer_, length(link))
  for (i in todo) {
    cand <- free_by_parent[[as.character(parent_g[i])]]
    if (length(cand) == 0) next
    hit <- contained(fold_g[i], ascii_ref[cand]) | contained(fold_g[i], std_ref[cand]) |
      vapply(cand, function(j) {
        s <- std_ref[j]
        !is.na(s) && nchar(s) > 3 && contained(s, fold_g[i])
      }, logical(1))
    if (sum(hit) == 1) candidate[i] <- cand[hit]
  }
  # the pairing must be the only one possible the other way round as well
  candidate[candidate %in% candidate[duplicated(candidate)]] <- NA_integer_
  gnrs_link_fill(link, candidate, alias)
}

#' Words that tell divisions apart rather than name one division twice
#' @keywords internal
#' @noRd
gnrs_qualifier_words <- function() {
  c(
    "north", "south", "east", "west", "northern", "southern", "eastern", "western",
    "central", "upper", "lower", "inner", "outer", "urban", "rural", "city", "town",
    "new", "old", "greater", "little", "big", "norte", "sur", "este", "oeste",
    "nord", "sud", "est", "ouest", "alto", "alta", "bajo", "baja", "nuevo", "nueva",
    "novo", "nova", "velho", "velha", "viejo", "vieja", "oriental", "occidental",
    "septentrional", "meridional", "centro", "ciudad", "cidade", "ville", "stadt",
    "land", "kreis", "ost", "west", "nordost", "nordwest", "sudost", "sudwest",
    "shi", "gun", "gu", "si", "district", "county", "municipality", "province"
  )
}

#' Do the names of a link agree outright?
#'
#' Internal.  For each GADM row with a link, whether its name and the
#' reference row's agree exactly (folded, unbracketed, reduced or by
#' alternate name); FALSE for unlinked rows.
#' @keywords internal
#' @noRd
gnrs_exact_link <- function(link, fold_g, fold_g2, reduced_g, alt_hit, ascii_ref, std_ref, reduced_ref) {
  out <- logical(length(link))
  i <- which(!is.na(link))
  if (length(i) == 0) return(out)
  j <- link[i]
  out[i] <- gnrs_names_agree(fold_g[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j], strict = TRUE) |
    gnrs_names_agree(fold_g2[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j], strict = TRUE) |
    (!is.na(alt_hit[i]) & alt_hit[i] == j)
  out
}

#' Withdraw links whose point lies too far from the polygon
#'
#' Internal.  \code{distances} was measured on an earlier pass; a pair not in
#' it is left alone.  A link whose names agree outright is kept whatever the
#' distance.
#' @keywords internal
#' @noRd
gnrs_spatial_veto <- function(link, id_ref, gid_g, exact, distances, level, max_km) {
  if (is.null(distances) || nrow(distances) == 0) return(link)
  d <- distances[distances$level == level, , drop = FALSE]
  i <- which(!is.na(link) & !exact)
  if (length(i) == 0) return(link)
  km <- d$km[match(paste(id_ref[link[i]], gid_g[i]), paste(d$geonameid, d$gid))]
  link[i[!is.na(km) & km > max_km]] <- NA_integer_
  link
}

#' Is a recorded GADM identifier to be believed?
#'
#' Internal.  \code{candidate} is, for each GADM row, the reference row that
#' carries its identifier, or NA.  It is believed when the HASC codes agree,
#' or when the names agree (folded, standardised or reduced, one being whole
#' words of the other, or the GADM name cut off at 32 characters beginning
#' the reference name), or, where either side lacks a HASC code, when the
#' names are at least somewhat alike under the same parent (trigram
#' similarity 0.2).  Whatever the codes say, it is not believed when the
#' names do not agree and a different GADM row bears the reference row's
#' name (\code{named_by}), or when a different GeoNames row bears the GADM
#' row's name (\code{claimant}): the name then decides, in a later step.  GADM has
#' reused identifiers for different divisions between releases, has renamed
#' divisions and mislabelled a few, and the service's own identifiers and
#' HASC codes are wrong here and there, so no single field is trusted alone.
#' @keywords internal
#' @noRd
gnrs_believe_identifier <- function(candidate, hasc_g, hasc_ref, fold_g, reduced_g, alt_hit, ascii_ref, std_ref, reduced_ref, same_parent, named_by = NULL, fold_g2 = fold_g, claimant = NULL) {
  i <- which(!is.na(candidate))
  if (length(i) == 0) return(candidate)
  j <- candidate[i]
  exact <- gnrs_names_agree(fold_g[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j], strict = TRUE) |
    gnrs_names_agree(fold_g2[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j], strict = TRUE) |
    (!is.na(alt_hit[i]) & alt_hit[i] == j)
  agree <- exact | gnrs_names_agree(fold_g[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j])
  alike <- agree
  parent_ok <- same_parent[i] %in% TRUE
  check <- which(!alike & parent_ok)
  if (length(check) > 0) {
    sim <- pmax(
      mapply(gnrs_similarity_pair, fold_g[i[check]], std_ref[j[check]], USE.NAMES = FALSE),
      mapply(gnrs_similarity_pair, fold_g[i[check]], ascii_ref[j[check]], USE.NAMES = FALSE)
    )
    alike[check] <- sim >= 0.2
  }
  both_hasc <- !is.na(hasc_g[i]) & !is.na(hasc_ref[j])
  believe <- ifelse(both_hasc, hasc_g[i] == hasc_ref[j] | agree, alike)
  if (!is.null(named_by)) {
    contested <- !is.na(named_by[j]) & named_by[j] != i
    believe[!exact & contested] <- FALSE
  }
  if (!is.null(claimant)) {
    claimed <- !is.na(claimant[i]) & claimant[i] != j
    believe[!exact & claimed] <- FALSE
  }
  candidate[i[!believe]] <- NA_integer_
  candidate
}

#' Is either of two names whole words of the other?
#' @keywords internal
#' @noRd
gnrs_pair_contains <- function(a, b) {
  out <- logical(length(a))
  ok <- !is.na(a) & !is.na(b) & nchar(a) > 3 & nchar(b) > 3
  pa <- paste0(" ", a[ok], " ")
  pb <- paste0(" ", b[ok], " ")
  out[ok] <- mapply(
    function(x, y) grepl(x, y, fixed = TRUE) || grepl(y, x, fixed = TRUE),
    pa, pb, USE.NAMES = FALSE
  )
  out
}

#' Do a GADM name and a reference name agree?
#'
#' Internal.  Agreement is equality of the folded GADM name with the
#' reference name or its standardised form, equality of the reduced names,
#' a GADM name cut off at 32 characters beginning the reference name, and,
#' unless \code{strict}, one name being whole words of the other.
#' @keywords internal
#' @noRd
gnrs_names_agree <- function(fold_g, reduced_g, ascii_ref, std_ref, reduced_ref, strict = FALSE) {
  agree <- fold_g == ascii_ref | fold_g == std_ref |
    (!is.na(reduced_g) & !is.na(reduced_ref) & reduced_g == reduced_ref) |
    (nchar(fold_g) >= 30 & (startsWith(ifelse(is.na(ascii_ref), "", ascii_ref), fold_g) | startsWith(ifelse(is.na(std_ref), "", std_ref), fold_g)))
  if (!strict) {
    agree <- agree | gnrs_pair_contains(fold_g, std_ref) | gnrs_pair_contains(fold_g, ascii_ref)
  }
  agree[is.na(agree)] <- FALSE
  agree
}

#' Withhold candidates whose names disagree while the name points elsewhere
#' @keywords internal
#' @noRd
gnrs_uncontested <- function(candidate, fold_g, reduced_g, alt_hit, ascii_ref, std_ref, reduced_ref, named_by, fold_g2 = fold_g, claimant = NULL) {
  i <- which(!is.na(candidate))
  if (length(i) == 0) return(candidate)
  j <- candidate[i]
  exact <- gnrs_names_agree(fold_g[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j], strict = TRUE) |
    gnrs_names_agree(fold_g2[i], reduced_g[i], ascii_ref[j], std_ref[j], reduced_ref[j], strict = TRUE) |
    (!is.na(alt_hit[i]) & alt_hit[i] == j)
  contested <- !is.na(named_by[j]) & named_by[j] != i
  if (!is.null(claimant)) contested <- contested | (!is.na(claimant[i]) & claimant[i] != j)
  candidate[i[!exact & contested]] <- NA_integer_
  candidate
}

#' Which GeoNames row under the GADM division's own parent bears its name
#' @keywords internal
#' @noRd
gnrs_geoname_named <- function(key_ascii_ref, key_std_ref, reduced_ref, is_geoname, key_g, reduced_g, alt_hit = NULL, key_g2 = key_g) {
  drop <- !(is_geoname %in% TRUE)
  key_ascii_ref[drop] <- NA_character_
  key_std_ref[drop] <- NA_character_
  reduced_ref[drop] <- NA_character_
  safe_match <- function(x, table) {
    m <- match(x, table)
    m[is.na(x)] <- NA_integer_
    m
  }
  out <- pmin(
    safe_match(key_g, key_ascii_ref), safe_match(key_g, key_std_ref), safe_match(reduced_g, reduced_ref),
    safe_match(key_g2, key_ascii_ref), safe_match(key_g2, key_std_ref),
    na.rm = TRUE
  )
  if (!is.null(alt_hit)) out <- pmin(out, alt_hit, na.rm = TRUE)
  out
}

#' A folded name with its words sorted: "thanh pho cao lanh" and "cao lanh
#' thanh pho" become the same string
#' @keywords internal
#' @noRd
gnrs_sort_words <- function(x) {
  words <- strsplit(x, "[^\\p{L}\\p{N}]+", perl = TRUE)
  out <- vapply(words, function(w) {
    w <- sort(w[!is.na(w) & nzchar(w)])
    if (length(w) < 2) NA_character_ else paste(w, collapse = " ")
  }, character(1))
  out
}

#' A folded name with its brackets dropped: "osh (city)" becomes "osh city"
#' @keywords internal
#' @noRd
gnrs_unbracket <- function(x) {
  out <- gsub("[[:space:]]+", " ", gsub("[][(){}]", " ", x))
  out <- trimws(out)
  out[!is.na(out) & out == x] <- NA_character_
  out
}

#' The GeoNames alternate names of the reference rows, as keys under the parent
#'
#' Internal.  Returns a data.frame of \code{key} (parent and folded alternate
#' name) and \code{row} (the reference row the name belongs to, through the
#' alias), keeping only keys that name one row under the parent.  NULL when
#' there are no alternate names.
#' @keywords internal
#' @noRd
gnrs_alt_keys <- function(altnames, ids, parent, alias) {
  if (is.null(altnames) || nrow(altnames) == 0) return(NULL)
  row <- match(altnames$geonameid, ids)
  ok <- !is.na(row)
  if (!any(ok)) return(NULL)
  row <- alias[row[ok]]
  k <- gnrs_key(parent[row], gnrs_lower(gnrs_unaccent(altnames$name[ok])))
  out <- unique(data.frame(key = k, row = row, stringsAsFactors = FALSE))
  out <- out[!is.na(out$key), , drop = FALSE]
  # a name shared by two rows under one parent names neither
  out[!(out$key %in% out$key[duplicated(out$key)]), , drop = FALSE]
}

#' The reference row whose alternate name a GADM name is, if exactly one
#' @keywords internal
#' @noRd
gnrs_alt_match <- function(key_g, alt) {
  if (is.null(alt)) return(rep(NA_integer_, length(key_g)))
  hit <- alt$row[match(key_g, alt$key)]
  hit[is.na(key_g)] <- NA_integer_
  # a GADM name borne by two GADM rows under one parent names neither
  hit[key_g %in% key_g[duplicated(key_g)]] <- NA_integer_
  hit
}

#' Redirect a candidate that is one of the service's copies to the GeoNames
#' row of the same name under the GADM division's parent, where there is one
#' @keywords internal
#' @noRd
gnrs_prefer_geoname <- function(candidate, is_geoname, geo_named) {
  swap <- !is.na(candidate) & !(is_geoname[candidate] %in% TRUE) & !is.na(geo_named)
  candidate[swap] <- geo_named[swap]
  candidate
}

#' Which GADM row bears each reference row's name
#'
#' Internal.  For every reference row, the index of the GADM row under the
#' same parent whose folded name equals the reference name, its standardised
#' form or its reduced form, or NA.  Used to withhold belief from an
#' identifier whose names do not agree while the name points elsewhere.
#' @keywords internal
#' @noRd
gnrs_named_by <- function(key_ascii_ref, key_std_ref, reduced_ref, key_g, reduced_g, alt_hit = NULL, key_g2 = key_g) {
  safe_match <- function(x, table) {
    m <- match(x, table)
    m[is.na(x)] <- NA_integer_
    m
  }
  out <- pmin(
    safe_match(key_ascii_ref, key_g), safe_match(key_std_ref, key_g), safe_match(reduced_ref, reduced_g),
    safe_match(key_ascii_ref, key_g2), safe_match(key_std_ref, key_g2),
    na.rm = TRUE
  )
  if (!is.null(alt_hit)) {
    # the GADM row whose name is an alternate name of the reference row
    by_alt <- match(seq_along(out), alt_hit)
    out <- pmin(out, by_alt, na.rm = TRUE)
  }
  out
}

#' @keywords internal
#' @noRd
gnrs_step_count <- function(steps, name, link) {
  steps[name] <- sum(!is.na(link)) - sum(steps)
  steps
}

#' Keys that occur more than once are no key at all
#' @keywords internal
#' @noRd
gnrs_unique_key <- function(k) {
  k[k %in% k[duplicated(k)]] <- NA_character_
  k
}

#' Names reduced to their distinguishing words
#'
#' Internal.  A name is accent-stripped, case-folded and split into words;
#' single letters, particles and the division-type words are dropped, and
#' what remains is joined again.  "Gemeente Zutphen" and "Zutphen", or
#' "Shahrestan-e Fasa" and "Fasa", reduce to the same string.  The type words
#' are those GADM uses for the divisions of the same country, plus a short
#' general list, so both sides of a comparison lose the same words.
#'
#' @param name Names.
#' @param group Country identifier of each name.
#' @param type_words A list of word vectors by country identifier, from
#'   \code{gnrs_type_words()}.
#' @return The reduced names; NA where nothing is left.
#' @keywords internal
#' @noRd
gnrs_reduce_name <- function(name, group, type_words) {
  words <- strsplit(gnrs_lower(gnrs_unaccent(name)), "[^\\p{L}\\p{N}]+", perl = TRUE)
  generic <- gnrs_generic_words()
  out <- rep(NA_character_, length(name))
  for (grp in unique(group[!is.na(group)])) {
    i <- which(group %in% grp)
    remove <- c(generic, type_words[[as.character(grp)]])
    out[i] <- vapply(words[i], function(p) {
      p <- p[!is.na(p) & nchar(p) > 1 & !(p %in% remove)]
      if (length(p) == 0) NA_character_ else paste(p, collapse = " ")
    }, character(1))
  }
  out
}

#' The type words GADM uses, by country
#' @keywords internal
#' @noRd
gnrs_type_words <- function(g, group) {
  text <- paste(ifelse(is.na(g$type), "", g$type), ifelse(is.na(g$engtype), "", g$engtype))
  words <- strsplit(gnrs_lower(gnrs_unaccent(text)), "[^\\p{L}\\p{N}]+", perl = TRUE)
  lapply(split(words, group), function(w) {
    w <- unique(unlist(w, use.names = FALSE))
    w[!is.na(w) & nchar(w) > 1]
  })
}

#' Particles and division words dropped when names are reduced
#' @keywords internal
#' @noRd
gnrs_generic_words <- function() {
  c(
    "de", "del", "della", "delle", "dei", "degli", "di", "do", "da", "dos", "das",
    "du", "des", "la", "le", "les", "el", "lo", "los", "las", "of", "the", "and",
    "und", "von", "van", "het", "der", "den", "al", "an",
    "district", "distrito", "distrikt", "municipality", "municipio", "municipalidad",
    "county", "province", "provincia", "provincie", "region", "regione", "department",
    "departamento", "departement", "commune", "comuna", "comune", "gemeente", "kreis",
    "landkreis", "kreisfreie", "stadt", "rayon", "raion", "rajon", "okrug", "oblast",
    "gorod", "horad", "shahri", "tumani", "qalasy", "srok", "shahrestan", "amphoe",
    "kabupaten", "kota", "borough", "parish", "canton", "arrondissement", "prefecture",
    "bezirk", "powiat", "gmina", "local", "government", "area", "regional", "autonomous",
    "autonoma", "autonomo", "capital", "federal", "territory", "territorio", "division",
    "council", "unitary", "authority", "administrative", "township"
  )
}

#' Measure each linked division's point against its GADM polygon
#'
#' Internal.  For every reference state and county linked to a GADM division
#' and with GeoNames coordinates, the distance in kilometres from the point to
#' the polygon it was linked to (0 inside).  The polygons are read from the
#' GeoPackage with sf one country at a time, through a spatial filter around
#' that country's points, which uses the file's index; the pieces of a
#' division (GADM's rows are its lowest-level areas) are dissolved first.  A
#' polygon that lies outside the filtered window is more than a degree from
#' the point and is recorded as 999 km.
#'
#' @param gpkg Path to the GeoPackage.
#' @param dir Cache directory holding the reference tables of a first linking
#'   pass and the coordinates.
#' @param quiet Suppress progress messages?
#' @return A data.frame: geonameid, level, gid, km.
#' @keywords internal
#' @noRd
gnrs_measure_links <- function(gpkg, dir = gnrs_cache_dir(), quiet = FALSE) {
  points <- as.data.frame(nanoparquet::read_parquet(gnrs_points_path(dir)))
  state <- as.data.frame(nanoparquet::read_parquet(gnrs_reference_path("state_province", dir)))
  county <- as.data.frame(nanoparquet::read_parquet(gnrs_reference_path("county_parish", dir)))
  pairs <- rbind(
    data.frame(geonameid = state$state_province_id, level = "state_province", gid = state$gid_1, stringsAsFactors = FALSE)[!is.na(state$gid_1) & state$is_geoname %in% TRUE, ],
    data.frame(geonameid = county$county_parish_id, level = "county_parish", gid = county$gid_2, stringsAsFactors = FALSE)[!is.na(county$gid_2) & county$is_geoname %in% TRUE, ]
  )
  m <- match(pairs$geonameid, points$geonameid)
  pairs <- pairs[!is.na(m), , drop = FALSE]
  pairs$lat <- points$lat[m[!is.na(m)]]
  pairs$lon <- points$lon[m[!is.na(m)]]
  pairs$km <- NA_real_
  if (nrow(pairs) == 0) return(pairs[c("geonameid", "level", "gid", "km")])

  table <- {
    con <- DBI::dbConnect(RSQLite::SQLite(), gpkg, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    tables <- DBI::dbListTables(con)
    tables[!grepl("^(gpkg_|rtree_|sqlite_)", tables)][1]
  }
  old_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  country <- substr(pairs$gid, 1, 3)
  t0 <- Sys.time()
  for (iso3 in unique(country)) {
    i <- which(country == iso3)
    bb <- c(min(pairs$lon[i]) - 1, max(-90, min(pairs$lat[i]) - 1), max(pairs$lon[i]) + 1, min(90, max(pairs$lat[i]) + 1))
    wkt <- sprintf(
      "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
      bb[1], bb[2], bb[3], bb[2], bb[3], bb[4], bb[1], bb[4], bb[1], bb[2]
    )
    polys <- tryCatch(
      suppressWarnings(sf::st_read(
        gpkg, query = paste0('SELECT GID_1, GID_2, geom FROM "', table, '"'),
        wkt_filter = wkt, quiet = TRUE
      )),
      error = function(e) NULL
    )
    pairs$km[i] <- 999
    if (is.null(polys) || nrow(polys) == 0) next
    pts <- sf::st_as_sf(pairs[i, ], coords = c("lon", "lat"), crs = 4326)
    for (level in unique(pairs$level[i])) {
      j <- i[pairs$level[i] == level]
      column <- if (level == "state_province") "GID_1" else "GID_2"
      keep <- !is.na(polys[[column]])
      if (!any(keep)) next
      dissolved <- suppressMessages(aggregate(polys[keep, column], by = list(gid = polys[[column]][keep]), FUN = function(x) x[1]))
      hit <- match(pairs$gid[j], dissolved$gid)
      ok <- !is.na(hit)
      if (any(ok)) {
        d <- suppressMessages(sf::st_distance(pts[match(j[ok], i), ], dissolved[hit[ok], ], by_element = TRUE))
        pairs$km[j[ok]] <- as.numeric(d) / 1000
      }
    }
    if (!quiet) message("  ", iso3, ": ", length(i), " divisions measured (", format(round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)), " min)")
  }
  pairs[c("geonameid", "level", "gid", "km")]
}

#' Put the spaces back into a GADM country name
#'
#' Internal.  Some GADM exports write "UnitedStates" and "SouthAfrica".  A
#' space is inserted before each capital that follows a lower-case letter,
#' which restores the common cases; a name that already has spaces is left
#' alone.
#' @keywords internal
#' @noRd
gnrs_gadm_country_name <- function(x) {
  squashed <- !is.na(x) & !grepl(" ", x, fixed = TRUE)
  x[squashed] <- gsub("([a-z])([A-Z])", "\\1 \\2", x[squashed])
  x
}
