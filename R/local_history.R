#' Historical political divisions: the CShapes component
#'
#' Internal.  Part of the time-versioned division reference shared by GNRS and
#' GVS (dev_notes/03 and 04).  Either package can build it; the code here is kept
#' identical in both.
#'
#' CShapes 2.0 (Schvitz et al. 2022) gives the borders of independent states and
#' dependencies from 1886 to 2019, one row per country-period with start and end
#' dates.  It codes STATES, not names: Gleditsch & Ward code 365 "Russia (Soviet
#' Union)" runs from 1886 to 2019 in 24 periods, so the Russian Empire, the USSR
#' and the Russian Federation share it.  Entities (the USSR as a named division)
#' are assigned to periods by the crosswalk, not here.
#'
#' Licence: CC BY-NC-SA 4.0.  The data are read from the `cshapes` package when
#' it is installed, otherwise downloaded on the user's machine; nothing derived is
#' shipped with the package.
#' @keywords internal
#' @noRd
gnrs_cshapes_spec <- function() {
  list(
    source = "cshapes",
    full_name = "CShapes 2.0 historical state boundaries",
    publisher = "International Conflict Research, ETH Zurich",
    version = "2.0",
    url = "https://icr.ethz.ch/data/cshapes/CShapes-2.0.geojson",
    license = "CC BY-NC-SA 4.0",
    citation = paste(
      "Schvitz G., Girardin L., Ruegger S., Weidmann N. B., Cederman L.-E. &",
      "Gleditsch K. S. (2022). Mapping the International System, 1886-2019:",
      "The CShapes 2.0 Dataset. Journal of Conflict Resolution 66(1): 144-161.",
      "https://doi.org/10.1177/00220027211013563"
    ),
    download_mb = 60,
    disk_mb = 25
  )
}

#' @keywords internal
#' @noRd
gnrs_cshapes_versions_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "cshapes-versions.gz.parquet")
}

#' @keywords internal
#' @noRd
gnrs_cshapes_geom_path <- function(dir = gnrs_cache_dir()) {
  file.path(dir, "cshapes-geometry.gpkg")
}

#' Read CShapes 2.0 as sf, from the cshapes package or the publisher
#' @keywords internal
#' @noRd
gnrs_read_cshapes <- function(dir = gnrs_cache_dir(), quiet = FALSE) {
  if (requireNamespace("cshapes", quietly = TRUE)) {
    if (!quiet) message("Reading CShapes 2.0 from the cshapes package ...")
    return(cshapes::cshp(date = NA, useGW = TRUE, dependencies = TRUE))
  }
  spec <- gnrs_cshapes_spec()
  f <- file.path(dir, "cshapes-2.0.geojson")
  if (!file.exists(f)) {
    if (!quiet) message("Downloading ", spec$full_name, " (about ", spec$download_mb, " MB) ...")
    partial <- paste0(f, ".part")
    # R's default of 60 seconds is not enough for a file this size
    old <- options(timeout = max(3600, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    status <- utils::download.file(spec$url, partial, mode = "wb", quiet = quiet, cacheOK = FALSE)
    if (status != 0 || !file.exists(partial)) {
      unlink(partial)
      stop("Download failed for CShapes.", call. = FALSE)
    }
    if (!file.rename(partial, f)) {
      unlink(partial)
      stop("Could not move the downloaded CShapes file into place.", call. = FALSE)
    }
  }
  x <- sf::st_read(f, quiet = TRUE)
  # the geojson spells the Gleditsch & Ward code and dates like the package does
  x$start <- as.Date(x$gwsdate %||% x$start)
  x$end <- as.Date(x$gwedate %||% x$end)
  x
}

#' Build the CShapes component: one row per country-period, with its six centroids
#'
#' Internal.  Writes
#' \itemize{
#'   \item \code{cshapes-versions.gz.parquet}: version_id, gwcode, country_name,
#'     valid_from, valid_to, status, owner, capital name and coordinates, b_def,
#'     area_km2, and the six planar centroids with their maximum distances
#'     (\code{c1_lon} ... \code{c6_dmax}), exactly as GVS computes them for GADM;
#'   \item \code{cshapes-geometry.gpkg}: the polygons, keyed by version_id.
#' }
#' @keywords internal
#' @noRd
gnrs_build_cshapes <- function(dir = gnrs_cache_dir(create = TRUE), quiet = FALSE) {
  for (pkg in c("sf", "nanoparquet")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Building the CShapes component needs the '", pkg, "' package.", call. = FALSE)
    }
  }
  x <- gnrs_read_cshapes(dir = dir, quiet = quiet)
  n <- nrow(x)
  version_id <- sprintf("cshapes:%d:%s", as.integer(x$gwcode), format(as.Date(x$start)))
  if (anyDuplicated(version_id)) stop("CShapes version ids are not unique.", call. = FALSE)

  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  # Repair with GEOS, not s2: s2's st_make_valid re-expresses polygons that cross the
  # antimeridian with longitudes beyond 180 (Russia's planar centroid came out at
  # 214.8 degrees instead of 96.8), which breaks point-in-polygon and the planar
  # centroids GVS compares against GADM.  s2 also rejects some unrepaired CShapes
  # rings (degenerate edges), and an ellipsoidal area with s2 off would need the
  # lwgeom package, so the area is taken in an equal-area projection with planar
  # geometry, which needs only PROJ.  Long straight edges in longitude/latitude
  # (the 129th meridian between Western and South Australia has two vertices)
  # are densified first, since a chord between distant vertices cuts area off
  # one side and adds it to the other once projected; the cylindrical equal-area
  # projection keeps meridians and parallels straight and gives the ellipsoidal
  # area.  Checked against lwgeom's ellipsoidal area over all 710 CShapes
  # country-periods: median ratio 1.0000, 99 percent within 0.1 percent, the
  # worst 0.4 percent (a diagonal border treated as a rhumb line, not a geodesic).
  suppressMessages(sf::sf_use_s2(FALSE))
  x <- sf::st_make_valid(x)
  area_km2 <- gnrs_equal_area_km2(sf::st_geometry(x))
  bb <- sf::st_bbox(x)
  if (bb[["xmin"]] < -180 || bb[["xmax"]] > 180 || bb[["ymin"]] < -90 || bb[["ymax"]] > 90) {
    stop("CShapes geometry outside WGS84 longitude/latitude bounds after repair.", call. = FALSE)
  }

  if (!quiet) message("Deriving centroids for ", n, " country-periods ...")
  cents <- gnrs_six_centroids(sf::st_geometry(x))

  attrs <- sf::st_drop_geometry(x)
  versions <- data.frame(
    version_id = version_id,
    gwcode = as.integer(attrs$gwcode),
    country_name = attrs$country_name,
    valid_from = as.Date(attrs$start),
    valid_to = as.Date(attrs$end),
    status = attrs$status,
    owner = suppressWarnings(as.integer(attrs$owner)),
    capital = attrs$capname,
    capital_lon = attrs$caplong,
    capital_lat = attrs$caplat,
    border_defined = as.integer(attrs$b_def),
    area_km2 = round(area_km2, 1),
    stringsAsFactors = FALSE
  )
  versions <- cbind(versions, cents)
  nanoparquet::write_parquet(versions, gnrs_cshapes_versions_path(dir), compression = "gzip")

  g <- sf::st_sf(version_id = version_id, geometry = sf::st_geometry(x))
  unlink(gnrs_cshapes_geom_path(dir))
  sf::st_write(g, gnrs_cshapes_geom_path(dir), layer = "country", quiet = TRUE)

  spec <- gnrs_cshapes_spec()
  provenance <- list(
    source = spec$source, full_name = spec$full_name, version = spec$version,
    url = spec$url, license = spec$license, citation = spec$citation,
    downloaded = as.character(Sys.Date()),
    via = if (requireNamespace("cshapes", quietly = TRUE)) {
      paste("cshapes package", as.character(utils::packageVersion("cshapes")))
    } else {
      "publisher download"
    },
    n_versions = n,
    n_states = length(unique(versions$gwcode)),
    first_date = as.character(min(versions$valid_from)),
    last_date = as.character(max(versions$valid_to))
  )
  saveRDS(provenance, gnrs_provenance_path("cshapes", dir))
  if (!quiet) {
    message("  ", n, " country-periods of ", provenance$n_states, " states, ",
            provenance$first_date, " to ", provenance$last_date)
  }
  invisible(provenance)
}

#' The six GVS centroids of each (multi)polygon
#'
#' Internal.  Identical to RGVS's \code{gvs_six_centroids()}: centre of mass,
#' point on surface and bounding-box centre, for the whole division and for its
#' largest part, each with the greatest planar distance from it to the WHOLE
#' division's vertices.  Planar WGS84 throughout, including across the
#' antimeridian, deliberately: GVS detects centroids that users computed naively.
#' @keywords internal
#' @noRd
gnrs_six_centroids <- function(geom) {
  n <- length(geom)
  out <- matrix(NA_real_, n, 18)
  main <- gnrs_largest_part(geom)
  for (half in 1:2) {
    g <- if (half == 1) geom else main
    cent <- suppressWarnings(sf::st_coordinates(sf::st_centroid(g)))
    pos <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(g)))
    bb <- vapply(g, function(x) {
      b <- sf::st_bbox(x)
      c((b[["xmin"]] + b[["xmax"]]) / 2, (b[["ymin"]] + b[["ymax"]]) / 2)
    }, numeric(2))
    types <- list(cent[, 1:2, drop = FALSE], pos[, 1:2, drop = FALSE], t(bb))
    for (k in seq_along(types)) {
      col <- (half - 1) * 9 + (k - 1) * 3 + 1
      out[, col] <- types[[k]][, 1]
      out[, col + 1] <- types[[k]][, 2]
      out[, col + 2] <- gnrs_max_vertex_distance(geom, types[[k]])
    }
  }
  colnames(out) <- paste0("c", rep(1:6, each = 3), c("_lon", "_lat", "_dmax"))
  as.data.frame(out)
}

#' @keywords internal
#' @noRd
gnrs_largest_part <- function(geom) {
  parts <- lapply(geom, function(g) {
    pieces <- suppressWarnings(sf::st_cast(sf::st_sfc(g), "POLYGON"))
    if (length(pieces) <= 1) {
      return(g)
    }
    areas <- suppressWarnings(as.numeric(sf::st_area(sf::st_set_crs(pieces, NA))))
    pieces[[which.max(areas)]]
  })
  sf::st_sfc(parts, crs = sf::st_crs(geom))
}

#' @keywords internal
#' @noRd
gnrs_max_vertex_distance <- function(geom, centres) {
  vapply(seq_along(geom), function(i) {
    xy <- sf::st_coordinates(sf::st_sfc(geom[[i]]))
    if (!nrow(xy) || any(is.na(centres[i, ]))) {
      return(NA_real_)
    }
    max(sqrt((xy[, 1] - centres[i, 1])^2 + (xy[, 2] - centres[i, 2])^2))
  }, numeric(1))
}

#' Area of longitude/latitude polygons without s2 or lwgeom
#'
#' Internal.  Planar densification of the edges (every 0.05 degrees, about
#' 5 km), then the cylindrical equal-area projection on the WGS84 ellipsoid
#' (EPSG:6933) and a planar area.  Used where s2 is switched off and the
#' lwgeom package cannot be assumed.
#'
#' @param geometry An sfc of polygons in longitude/latitude.
#' @return Areas in square kilometres.
#' @keywords internal
#' @noRd
gnrs_equal_area_km2 <- function(geometry) {
  planar <- sf::st_set_crs(geometry, NA)
  dense <- sf::st_segmentize(planar, 0.05)
  dense <- sf::st_set_crs(dense, 4326)
  projected <- sf::st_transform(dense, "EPSG:6933")
  as.numeric(sf::st_area(projected)) / 1e6
}
