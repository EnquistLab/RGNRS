#' Resolve a batch of political division names against the local reference
#'
#' Internal.  R port of the resolution cascade the GNRS runs in
#' \code{gnrs.sh}: a fixed sequence of SQL update scripts, each of which fills
#' in the rows the earlier ones left unresolved.  The order is the upstream
#' order and the match-method labels are the upstream labels, so that a local
#' result reads like one from the web service.
#'
#' The steps, in order: the alternate forms of the submitted state and county
#' are derived; the country is matched exactly, then fuzzily; a country that
#' is really a constituent country of the United Kingdom is recognised
#' (country-as-state); the state is matched exactly, then fuzzily; a state that
#' is really a country in the reference, such as Puerto Rico under the United
#' States, is recognised (state-as-country); the county is matched exactly,
#' then fuzzily; a state that is really a county of a constituent country
#' (state-as-county) and a county that is really a state of a state-as-country
#' (county-as-state) are recognised; and the result is summarised and scored.
#'
#' Where the upstream SQL would pick one of several equally good matches
#' arbitrarily, the one with the lowest identifier is taken, so that results
#' are reproducible.
#'
#' @param u A data.frame of distinct submissions with the columns
#'   \code{country_verbatim}, \code{state_province_verbatim} and
#'   \code{county_parish_verbatim}, blanks as "".
#' @param bb The backbone from \code{gnrs_backbone()}.
#' @param threshold Fuzzy-match threshold.
#' @return \code{u} with the resolution columns filled in.
#' @keywords internal
#' @noRd
gnrs_resolve <- function(u, bb, threshold = 0.5) {
  n <- nrow(u)
  na_chr <- rep(NA_character_, n)
  na_int <- rep(NA_integer_, n)
  na_num <- rep(NA_real_, n)

  u$state_province_verbatim_alt <- gnrs_state_verbatim_alt(u$state_province_verbatim)
  u$county_parish_verbatim_alt <- gnrs_county_verbatim_alt(u$county_parish_verbatim)
  u$country <- na_chr
  u$state_province <- na_chr
  u$county_parish <- na_chr
  u$country_id <- na_int
  u$state_province_id <- na_int
  u$county_parish_id <- na_int
  u$match_method_country <- na_chr
  u$match_method_state_province <- na_chr
  u$match_method_county_parish <- na_chr
  u$match_score_country <- na_num
  u$match_score_state_province <- na_num
  u$match_score_county_parish <- na_num

  if (n == 0) {
    return(gnrs_summarize(u, bb))
  }

  ctx <- list(bb = bb, threshold = threshold, memo = new.env(parent = emptyenv()))

  u <- gnrs_step_country_exact(u, ctx)
  u <- gnrs_step_country_fuzzy(u, ctx)
  u <- gnrs_step_countryasstate_exact(u, ctx)
  u <- gnrs_step_countryasstate_fuzzy(u, ctx)
  u <- gnrs_step_state_exact(u, ctx)
  u <- gnrs_step_state_fuzzy(u, ctx)
  u <- gnrs_step_stateascountry_exact(u, ctx)
  u <- gnrs_step_stateascountry_fuzzy(u, ctx)
  u <- gnrs_step_county_exact(u, ctx)
  u <- gnrs_step_county_fuzzy(u, ctx)
  u <- gnrs_step_stateascounty_exact(u, ctx)
  u <- gnrs_step_stateascounty_fuzzy(u, ctx)
  u <- gnrs_step_countyasstate_exact(u, ctx)
  u <- gnrs_step_countyasstate_fuzzy(u, ctx)

  gnrs_summarize(u, bb)
}

# ---------------------------------------------------------------------------
# Assignment helpers.  Names are always filled from the reference by
# identifier, as the SQL does; the state and county names reported are the
# standard (short ASCII) forms.
# ---------------------------------------------------------------------------

gnrs_set_country <- function(u, rows, ids, method, score = NULL, clear_state = FALSE) {
  if (length(rows) == 0) {
    return(u)
  }
  co <- gnrs_env$backbone$country
  m <- match(ids, co$country_id)
  u$country_id[rows] <- co$country_id[m]
  u$country[rows] <- co$country[m]
  u$match_method_country[rows] <- method
  if (!is.null(score)) u$match_score_country[rows] <- score
  if (clear_state) {
    u$state_province[rows] <- NA_character_
    u$state_province_id[rows] <- NA_integer_
  }
  u
}

gnrs_set_state <- function(u, rows, ids, method, score = NULL, clear_county = FALSE,
                           infer_country = FALSE) {
  if (length(rows) == 0) {
    return(u)
  }
  st <- gnrs_env$backbone$state
  m <- match(ids, st$state_province_id)
  u$state_province_id[rows] <- st$state_province_id[m]
  u$state_province[rows] <- st$state_province_std[m]
  u$match_method_state_province[rows] <- method
  if (!is.null(score)) u$match_score_state_province[rows] <- score
  if (infer_country) {
    co <- gnrs_env$backbone$country
    cm <- match(st$country_id[m], co$country_id)
    u$country_id[rows] <- co$country_id[cm]
    u$country[rows] <- co$country[cm]
    u$match_method_country[rows] <- "inferred from country-as-state"
  }
  if (clear_county) {
    u$county_parish[rows] <- NA_character_
    u$county_parish_id[rows] <- NA_integer_
  }
  u
}

gnrs_set_county <- function(u, rows, ids, method, score = NULL) {
  if (length(rows) == 0) {
    return(u)
  }
  ct <- gnrs_env$backbone$county
  m <- match(ids, ct$county_parish_id)
  u$county_parish_id[rows] <- ct$county_parish_id[m]
  u$county_parish[rows] <- ct$county_parish_std[m]
  u$match_method_county_parish[rows] <- method
  if (!is.null(score)) u$match_score_county_parish[rows] <- score
  u
}

#' Exact lookup of submitted keys against reference keys
#'
#' Internal.  \code{match()} returns the first reference row, and the reference
#' is ordered by identifier, so several reference rows with the same key
#' resolve to the lowest identifier.
#' @keywords internal
#' @noRd
gnrs_exact <- function(rows, key_u, key_ref, ids_ref) {
  if (length(rows) == 0) {
    return(list(rows = integer(0), ids = integer(0)))
  }
  m <- match(key_u, key_ref)
  hit <- !is.na(m) & !is.na(key_u)
  list(rows = rows[hit], ids = ids_ref[m[hit]])
}

#' Scoped composite key for a submitted value
#' @keywords internal
#' @noRd
gnrs_key <- function(parent, x, sep = "\r") {
  out <- paste0(parent, sep, x)
  out[is.na(parent) | is.na(x) | !nzchar(x)] <- NA_character_
  out
}

#' Best fuzzy match of one submitted value within a scope
#'
#' Internal.  The SQL takes the reference names whose similarity equals the
#' maximum, provided the maximum reaches the threshold, and updates with one
#' of them.  Memoised on the scope and the value, since the same submitted
#' name recurs across many rows.
#'
#' @return A list of \code{id} and \code{score}, or NULL when nothing reaches
#'   the threshold.
#' @keywords internal
#' @noRd
gnrs_fuzzy_best <- function(ctx, verbatim, what, parent = NA) {
  memo_key <- paste("fuzzy", what, parent, verbatim, sep = "\r")
  hit <- ctx$memo[[memo_key]]
  if (!is.null(hit)) {
    return(if (identical(hit, FALSE)) NULL else hit)
  }
  scope <- gnrs_scope(ctx$bb, what, parent, index = TRUE)
  out <- NULL
  if (length(scope$ids) > 0) {
    q <- gnrs_trigrams(verbatim)[[1]]
    s <- gnrs_similarity(q, scope$index)
    best <- max(s)
    if (best >= ctx$threshold) {
      out <- list(id = min(scope$ids[s == best]), score = gnrs_numeric2(best))
    }
  }
  ctx$memo[[memo_key]] <- if (is.null(out)) FALSE else out
  out
}

#' Wildcard match of one submitted value within a scope
#'
#' Internal.  Either the reference name contains the submitted value or the
#' value contains the name; the SQL accepts the result only when exactly one
#' reference identifier is matched.  Memoised like the fuzzy lookup.
#'
#' @return The single matched identifier, or NA.
#' @keywords internal
#' @noRd
gnrs_wildcard_unique <- function(ctx, verbatim, what, parent = NA) {
  memo_key <- paste("wild", what, parent, verbatim, sep = "\r")
  hit <- ctx$memo[[memo_key]]
  if (!is.null(hit)) {
    return(hit)
  }
  scope <- gnrs_scope(ctx$bb, what, parent)
  out <- NA_integer_
  if (length(scope$ids) > 0) {
    ids <- unique(scope$ids[gnrs_either_contains(scope$names, verbatim)])
    if (length(ids) == 1) out <- ids
  }
  ctx$memo[[memo_key]] <- out
  out
}

#' The guards every wildcard step applies to the submitted value
#'
#' Internal.  Non-blank, no underscore (a LIKE wildcard), not a lone hyphen,
#' and longer than three characters, to keep short values from matching
#' codes.
#' @keywords internal
#' @noRd
gnrs_wildcard_ok <- function(x) {
  !gnrs_blank(x) & !grepl("_", x, fixed = TRUE) & x != "-" & nchar(x) > 3
}

# ---------------------------------------------------------------------------
# Country
# ---------------------------------------------------------------------------

gnrs_step_country_exact <- function(u, ctx) {
  co <- ctx$bb$country
  cn <- ctx$bb$country_names
  cv <- u$country_verbatim
  cv_lower <- gnrs_lower(cv)
  cv_ascii <- gnrs_unaccent(cv)
  cv_ascii_lower <- gnrs_lower(cv_ascii)

  tries <- list(
    list(cv, co$country, co$country_id, "exact standard name"),
    list(cv_lower, co$lower, co$country_id, "exact standard name"),
    list(cv_ascii, co$country, co$country_id, "exact ascii name"),
    list(cv_ascii_lower, co$lower, co$country_id, "exact ascii name"),
    list(cv, co$iso, co$country_id, "iso code"),
    list(cv, co$iso_alpha3, co$country_id, "iso_alpha3 code"),
    list(cv, cn$name[cn$original], cn$id[cn$original], "exact alternate name")
  )
  for (t in tries) {
    rows <- which(is.na(u$country_id) & is.na(u$match_method_country) & !gnrs_blank(cv))
    hit <- gnrs_exact(rows, t[[1]][rows], t[[2]], t[[3]])
    u <- gnrs_set_country(u, hit$rows, hit$ids, t[[4]])
  }
  u
}

gnrs_step_country_fuzzy <- function(u, ctx) {
  for (t in list(c("country_std", "fuzzy standard name"), c("country_alt", "fuzzy alternate name"))) {
    rows <- which(is.na(u$country_id) & is.na(u$match_method_country) & !gnrs_blank(u$country_verbatim))
    for (i in rows) {
      best <- gnrs_fuzzy_best(ctx, u$country_verbatim[i], t[1])
      if (!is.null(best)) {
        u <- gnrs_set_country(u, i, best$id, t[2], score = best$score)
      }
    }
  }
  u
}

# ---------------------------------------------------------------------------
# Country-as-state: a submitted country that is a constituent country of the
# United Kingdom.  The country is inferred from the state.
# ---------------------------------------------------------------------------

gnrs_step_countryasstate_exact <- function(u, ctx) {
  st <- ctx$bb$state
  cas <- which(st$is_countryasstate)
  if (length(cas) == 0) {
    return(u)
  }
  sn <- ctx$bb$state_names
  cas_names <- sn[sn$id %in% st$state_province_id[cas], , drop = FALSE]
  cv <- u$country_verbatim
  cv_ascii_lower <- gnrs_lower(gnrs_unaccent(cv))

  codes <- c(
    "state_province_code", "state_province_code_full", "hasc", "hasc_full",
    "state_province_code2", "state_province_code2_full"
  )
  code_keys <- unlist(lapply(codes, function(col) st[[col]][cas]), use.names = FALSE)
  code_ids <- rep(st$state_province_id[cas], length(codes))
  keep <- !is.na(code_keys) & nzchar(code_keys)
  # Ordered by identifier so that match() picks the lowest
  ord <- order(code_ids[keep])

  tries <- list(
    list(cv_ascii_lower, st$lower_name[cas], st$state_province_id[cas], "exact standard name, country-as-state"),
    list(cv, code_keys[keep][ord], code_ids[keep][ord], "hasc/fips/iso codes, country-as-state"),
    list(cv, cas_names$name, cas_names$id, "exact alternate name, country-as-state")
  )
  for (t in tries) {
    rows <- which(is.na(u$country_id) & !gnrs_blank(cv))
    hit <- gnrs_exact(rows, t[[1]][rows], t[[2]], t[[3]])
    u <- gnrs_set_state(u, hit$rows, hit$ids, t[[4]], infer_country = TRUE)
  }
  u
}

gnrs_step_countryasstate_fuzzy <- function(u, ctx) {
  cv <- u$country_verbatim

  rows <- which(is.na(u$country_id) & gnrs_wildcard_ok(cv))
  for (i in rows) {
    id <- gnrs_wildcard_unique(ctx, cv[i], "cas_alt")
    if (!is.na(id)) {
      u <- gnrs_set_state(u, i, id, "wildcard alt name, country-as-state", infer_country = TRUE)
    }
  }

  for (what in c("cas_name", "cas_ascii", "cas_std")) {
    rows <- which(is.na(u$country_id) & !gnrs_blank(cv))
    for (i in rows) {
      best <- gnrs_fuzzy_best(ctx, cv[i], what)
      if (!is.null(best)) {
        u <- gnrs_set_state(u, i, best$id, "fuzzy standard name, country-as-state",
          score = best$score, infer_country = TRUE
        )
      }
    }
  }
  u
}

# ---------------------------------------------------------------------------
# State/province, within the matched country
# ---------------------------------------------------------------------------

gnrs_step_state_exact <- function(u, ctx) {
  st <- ctx$bb$state
  sn <- ctx$bb$state_names
  sv <- u$state_province_verbatim
  sv_lower <- gnrs_lower(sv)
  sv_ascii <- gnrs_unaccent(sv)
  sv_ascii_lower <- gnrs_lower(sv_ascii)
  cid <- u$country_id

  tries <- list(
    list(sv, "key_name", "exact name"),
    list(sv_lower, "key_lower_name", "exact name"),
    list(sv_ascii, "key_ascii", "exact ascii name"),
    list(sv_ascii_lower, "key_lower_ascii", "exact ascii name"),
    list(sv_ascii, "key_std", "exact ascii short name"),
    list(sv_ascii_lower, "key_lower_std", "exact ascii short name"),
    list(sv, "key_code_full", "full iso code"),
    list(sv, "key_hasc_full", "full hasc code"),
    list(sv, "key_code2_full", "full alternate code"),
    list(sv, "key_code", "iso code"),
    list(sv, "key_hasc", "hasc code"),
    list(sv, "key_code2", "alternate code")
  )
  for (t in tries) {
    rows <- which(!is.na(cid) & is.na(u$state_province_id) &
      is.na(u$match_method_state_province) & !gnrs_blank(sv))
    hit <- gnrs_exact(rows, gnrs_key(cid[rows], t[[1]][rows]), st[[t[[2]]]], st$state_province_id)
    u <- gnrs_set_state(u, hit$rows, hit$ids, t[[3]])
  }

  rows <- which(!is.na(cid) & is.na(u$state_province_id) &
    is.na(u$match_method_state_province) & !gnrs_blank(sv))
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], sv[rows]), sn$key_original, sn$id)
  gnrs_set_state(u, hit$rows, hit$ids, "exact alternate name")
}

gnrs_step_state_fuzzy <- function(u, ctx) {
  sv <- u$state_province_verbatim
  cid <- u$country_id
  todo <- function() {
    which(!is.na(cid) & is.na(u$state_province_id) &
      is.na(u$match_method_state_province) & !gnrs_blank(sv))
  }

  for (i in todo()) {
    if (!gnrs_wildcard_ok(sv[i])) next
    id <- gnrs_wildcard_unique(ctx, sv[i], "state_alt", cid[i])
    if (!is.na(id)) u <- gnrs_set_state(u, i, id, "wildcard alt name")
  }

  fuzzy <- list(
    c("state_name", "fuzzy standard name"),
    c("state_ascii", "fuzzy ascii name"),
    c("state_std", "fuzzy ascii short name"),
    c("state_alt_original", "fuzzy alternate name")
  )
  for (t in fuzzy) {
    for (i in todo()) {
      best <- gnrs_fuzzy_best(ctx, sv[i], t[1], cid[i])
      if (!is.null(best)) u <- gnrs_set_state(u, i, best$id, t[2], score = best$score)
    }
  }

  # The alternate form of the submitted state, stripped of words such as
  # "Province", against the alternate names.  Upstream requires the match to
  # be unique, as the other wildcard steps do; see the package notes on why
  # its own test does not achieve that.
  sv_alt <- u$state_province_verbatim_alt
  for (i in todo()) {
    if (gnrs_blank(sv_alt[i])) next
    id <- gnrs_wildcard_unique(ctx, sv_alt[i], "state_alt", cid[i])
    if (!is.na(id)) u <- gnrs_set_state(u, i, id, "wildcard alt verbatim name")
  }

  # A state name in the county field, when no state was submitted at all
  st <- ctx$bb$state
  # ILIKE upstream: compared on lower-cased copies, since a fixed search
  # cannot ignore case
  cpv_ascii_lower <- gnrs_lower(gnrs_unaccent(u$county_parish_verbatim))
  rows <- which(!is.na(cid) & is.na(u$state_province_id) & gnrs_blank(sv) &
    !gnrs_blank(u$county_parish_verbatim))
  for (i in rows) {
    srows <- ctx$bb$state_rows_by_country[[as.character(cid[i])]]
    if (is.null(srows)) next
    std <- st$lower_std[srows]
    ok <- !is.na(std) & nzchar(std)
    found <- ok
    found[ok] <- vapply(
      std[ok], function(s) grepl(s, cpv_ascii_lower[i], fixed = TRUE),
      logical(1), USE.NAMES = FALSE
    )
    if (any(found)) {
      u <- gnrs_set_state(u, i, min(st$state_province_id[srows][found]), "wildcard state-in-county-field")
    }
  }
  u
}

# ---------------------------------------------------------------------------
# State-as-country: a submitted state that the reference holds as a country
# (Puerto Rico under the United States).  The country becomes that country and
# the state is cleared.
# ---------------------------------------------------------------------------

gnrs_step_stateascountry_exact <- function(u, ctx) {
  co <- ctx$bb$country
  cn <- ctx$bb$country_names
  sv <- u$state_province_verbatim
  sv_lower <- gnrs_lower(sv)
  sv_ascii_lower <- gnrs_lower(gnrs_unaccent(sv))
  cid <- u$country_id

  sac <- which(!is.na(co$alt_country_id))
  if (length(sac) == 0) {
    return(u)
  }
  sac_names <- cn[cn$id %in% co$country_id[sac], , drop = FALSE]
  sac_names$parent <- co$alt_country_id[match(sac_names$id, co$country_id)]
  sac_names$lower <- gnrs_lower(sac_names$name)

  codes <- c("iso", "iso_alpha3", "fips")
  code_keys <- unlist(lapply(codes, function(col) gnrs_key(co$alt_country_id[sac], co[[col]][sac])), use.names = FALSE)
  code_ids <- rep(co$country_id[sac], length(codes))
  ord <- order(code_ids)

  todo <- function() which(!is.na(cid) & is.na(u$state_province_id) & !gnrs_blank(sv))

  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], sv_ascii_lower[rows]),
    gnrs_key(co$alt_country_id[sac], co$lower[sac]), co$country_id[sac])
  u <- gnrs_set_country(u, hit$rows, hit$ids, "exact standard name, state-as-country", clear_state = TRUE)
  cid <- u$country_id

  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], sv[rows]), code_keys[ord], code_ids[ord])
  u <- gnrs_set_country(u, hit$rows, hit$ids, "ISO/FIPS/HASC code, state-as-country", clear_state = TRUE)
  cid <- u$country_id

  rows <- todo()
  key_ref <- c(gnrs_key(sac_names$parent, sac_names$lower), gnrs_key(sac_names$parent, sac_names$name))
  ids_ref <- c(sac_names$id, sac_names$id)
  ord <- order(ids_ref)
  hit_lower <- gnrs_exact(rows, gnrs_key(cid[rows], sv_lower[rows]), key_ref[ord], ids_ref[ord])
  u <- gnrs_set_country(u, hit_lower$rows, hit_lower$ids, "exact alternate name, state-as-country", clear_state = TRUE)
  cid <- u$country_id
  rows <- todo()
  hit_exact <- gnrs_exact(rows, gnrs_key(cid[rows], sv[rows]), key_ref[ord], ids_ref[ord])
  gnrs_set_country(u, hit_exact$rows, hit_exact$ids, "exact alternate name, state-as-country", clear_state = TRUE)
}

gnrs_step_stateascountry_fuzzy <- function(u, ctx) {
  sv <- u$state_province_verbatim

  rows <- which(!is.na(u$country_id) & is.na(u$state_province_id) & gnrs_wildcard_ok(sv))
  for (i in rows) {
    id <- gnrs_wildcard_unique(ctx, sv[i], "sac_alt", u$country_id[i])
    if (!is.na(id)) {
      u <- gnrs_set_country(u, i, id, "wildcard alternate name, state-as-country", clear_state = TRUE)
    }
  }

  not_sac <- function() {
    which(!is.na(u$country_id) & is.na(u$state_province_id) & !gnrs_blank(sv) &
      !is.na(u$match_method_country) &
      !grepl("state-as-country", u$match_method_country, fixed = TRUE))
  }
  for (t in list(c("sac_std", "fuzzy standard name, state-as-country"),
                 c("sac_alt", "fuzzy alternate name, state-as-country"))) {
    for (i in not_sac()) {
      best <- gnrs_fuzzy_best(ctx, sv[i], t[1], u$country_id[i])
      if (!is.null(best)) {
        u <- gnrs_set_country(u, i, best$id, t[2], score = best$score, clear_state = TRUE)
      }
    }
  }
  u
}

# ---------------------------------------------------------------------------
# County/parish, within the matched state
# ---------------------------------------------------------------------------

gnrs_step_county_exact <- function(u, ctx) {
  ct <- ctx$bb$county
  cn <- ctx$bb$county_names
  cpv <- u$county_parish_verbatim
  cpv_lower <- gnrs_lower(cpv)
  cpv_ascii <- gnrs_unaccent(cpv)
  cpv_ascii_lower <- gnrs_lower(cpv_ascii)
  sid <- u$state_province_id

  tries <- list(
    list(cpv, "key_name", "exact name"),
    list(cpv_lower, "key_lower_name", "exact name"),
    list(cpv_ascii, "key_ascii", "exact ascii name"),
    list(cpv_ascii_lower, "key_lower_ascii", "exact ascii name"),
    list(cpv_ascii, "key_std", "exact ascii short name"),
    list(cpv_ascii_lower, "key_lower_std", "exact ascii short name"),
    list(cpv, "key_code_full", "full iso code"),
    list(cpv, "key_hasc_full", "full hasc code"),
    list(cpv, "key_code2_full", "full alternate code"),
    list(cpv, "key_code", "iso code"),
    list(cpv, "key_hasc", "hasc code"),
    list(cpv, "key_code2", "alternate code")
  )
  for (t in tries) {
    rows <- which(!is.na(u$country_id) & !is.na(sid) & is.na(u$county_parish_id) & !gnrs_blank(cpv))
    hit <- gnrs_exact(rows, gnrs_key(sid[rows], t[[1]][rows]), ct[[t[[2]]]], ct$county_parish_id)
    u <- gnrs_set_county(u, hit$rows, hit$ids, t[[3]])
  }

  rows <- which(!is.na(u$country_id) & !is.na(sid) & is.na(u$county_parish_id) & !gnrs_blank(cpv))
  hit <- gnrs_exact(rows, gnrs_key(sid[rows], cpv[rows]), cn$key_original, cn$id)
  gnrs_set_county(u, hit$rows, hit$ids, "exact alternate name")
}

gnrs_step_county_fuzzy <- function(u, ctx) {
  cpv <- u$county_parish_verbatim
  sid <- u$state_province_id
  todo <- function() {
    which(!is.na(u$country_id) & !is.na(sid) & is.na(u$county_parish_id) &
      is.na(u$match_method_county_parish) & !gnrs_blank(cpv))
  }

  for (i in todo()) {
    if (!gnrs_wildcard_ok(cpv[i])) next
    id <- gnrs_wildcard_unique(ctx, cpv[i], "county_alt", sid[i])
    if (!is.na(id)) u <- gnrs_set_county(u, i, id, "wildcard alt name")
  }

  fuzzy <- list(
    c("county_name", "fuzzy standard name"),
    c("county_ascii", "fuzzy ascii name"),
    c("county_std", "fuzzy ascii short name"),
    c("county_alt_original", "fuzzy alternate name")
  )
  for (t in fuzzy) {
    for (i in todo()) {
      best <- gnrs_fuzzy_best(ctx, cpv[i], t[1], sid[i])
      if (!is.null(best)) u <- gnrs_set_county(u, i, best$id, t[2], score = best$score)
    }
  }

  cpv_alt <- u$county_parish_verbatim_alt
  for (i in todo()) {
    if (gnrs_blank(cpv_alt[i])) next
    id <- gnrs_wildcard_unique(ctx, cpv_alt[i], "county_alt", sid[i])
    if (!is.na(id)) u <- gnrs_set_county(u, i, id, "wildcard alt verbatim name")
  }
  u
}

# ---------------------------------------------------------------------------
# State-as-county: under a constituent country of the United Kingdom, the
# submitted state is looked for among its counties.
# ---------------------------------------------------------------------------

gnrs_step_stateascounty_exact <- function(u, ctx) {
  st <- ctx$bb$state
  ct <- ctx$bb$county
  cn <- ctx$bb$county_names
  sv <- u$state_province_verbatim
  sv_ascii_lower <- gnrs_lower(gnrs_unaccent(sv))
  sid <- u$state_province_id

  sac <- which(ct$is_stateascounty)
  if (length(sac) == 0) {
    return(u)
  }
  cas_state <- st$state_province_id[st$is_countryasstate]
  sac_names <- cn[cn$id %in% ct$county_parish_id[sac], , drop = FALSE]

  todo <- function() {
    which(is.na(u$county_parish_id) & !is.na(sid) & sid %in% cas_state & !gnrs_blank(sv))
  }

  name_keys <- c(
    gnrs_key(ct$state_province_id[sac], ct$lower_name[sac]),
    gnrs_key(ct$state_province_id[sac], ct$lower_ascii[sac]),
    gnrs_key(ct$state_province_id[sac], ct$lower_std[sac])
  )
  name_ids <- rep(ct$county_parish_id[sac], 3)
  ord <- order(name_ids)
  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(sid[rows], sv_ascii_lower[rows]), name_keys[ord], name_ids[ord])
  u <- gnrs_set_county(u, hit$rows, hit$ids, "exact standard name, state-as-county")

  codes <- c(
    "county_parish_code", "county_parish_code_full", "hasc_2", "hasc_2_full",
    "county_parish_code2", "county_parish_code2_full"
  )
  code_keys <- unlist(lapply(codes, function(col) gnrs_key(ct$state_province_id[sac], ct[[col]][sac])), use.names = FALSE)
  code_ids <- rep(ct$county_parish_id[sac], length(codes))
  ord <- order(code_ids)
  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(sid[rows], sv[rows]), code_keys[ord], code_ids[ord])
  u <- gnrs_set_county(u, hit$rows, hit$ids, "hasc/fips/iso codes, state-as-county")

  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(sid[rows], sv[rows]),
    gnrs_key(sac_names$state_province_id, sac_names$name), sac_names$id)
  gnrs_set_county(u, hit$rows, hit$ids, "exact alternate name, state-as-county")
}

gnrs_step_stateascounty_fuzzy <- function(u, ctx) {
  sv <- u$state_province_verbatim
  sid <- u$state_province_id
  todo <- function() which(is.na(u$county_parish_id) & !is.na(sid) & !gnrs_blank(sv))

  for (i in todo()) {
    if (!gnrs_wildcard_ok(sv[i])) next
    id <- gnrs_wildcard_unique(ctx, sv[i], "sac_county_alt", sid[i])
    if (!is.na(id)) u <- gnrs_set_county(u, i, id, "wildcard alternate name, state-as-county")
  }
  for (what in c("sac_county_name", "sac_county_ascii", "sac_county_std")) {
    for (i in todo()) {
      best <- gnrs_fuzzy_best(ctx, sv[i], what, sid[i])
      if (!is.null(best)) {
        u <- gnrs_set_county(u, i, best$id, "fuzzy standard name, state-as-county", score = best$score)
      }
    }
  }
  u
}

# ---------------------------------------------------------------------------
# County-as-state: under a state-as-country, the submitted county is looked
# for among that country's states.
# ---------------------------------------------------------------------------

gnrs_is_sac_row <- function(u, ctx) {
  co <- ctx$bb$country
  has_alt <- !is.na(co$alt_country_id[match(u$country_id, co$country_id)])
  !is.na(u$country_id) & has_alt & !is.na(u$match_method_country) &
    grepl("state-as-country", u$match_method_country, fixed = TRUE)
}

gnrs_step_countyasstate_exact <- function(u, ctx) {
  st <- ctx$bb$state
  sn <- ctx$bb$state_names
  cpv <- u$county_parish_verbatim
  cpv_ascii_lower <- gnrs_lower(gnrs_unaccent(cpv))
  cid <- u$country_id

  todo <- function() which(gnrs_is_sac_row(u, ctx) & is.na(u$state_province_id) & !gnrs_blank(cpv))

  name_keys <- c(gnrs_key(st$country_id, st$lower_ascii), gnrs_key(st$country_id, st$lower_std))
  name_ids <- rep(st$state_province_id, 2)
  ord <- order(name_ids)
  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], cpv_ascii_lower[rows]), name_keys[ord], name_ids[ord])
  u <- gnrs_set_state(u, hit$rows, hit$ids, "exact standard name, county-as-state", clear_county = TRUE)

  fips_full <- gsub(".", "", st$state_province_code_full, fixed = TRUE)
  codes <- list(
    st$state_province_code, st$state_province_code_full, st$hasc, st$hasc_full,
    st$state_province_code2, st$state_province_code2_full, fips_full
  )
  code_keys <- unlist(lapply(codes, function(x) gnrs_key(st$country_id, x)), use.names = FALSE)
  code_ids <- rep(st$state_province_id, length(codes))
  ord <- order(code_ids)
  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], cpv[rows]), code_keys[ord], code_ids[ord])
  u <- gnrs_set_state(u, hit$rows, hit$ids, "ISO/FIPS/HASC code, county-as-state", clear_county = TRUE)

  alt_keys <- c(gnrs_key(sn$country_id, sn$lower), gnrs_key(sn$country_id, sn$name))
  alt_ids <- rep(sn$id, 2)
  ord <- order(alt_ids)
  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], cpv_ascii_lower[rows]), alt_keys[ord], alt_ids[ord])
  u <- gnrs_set_state(u, hit$rows, hit$ids, "exact standard name, county-as-state", clear_county = TRUE)
  rows <- todo()
  hit <- gnrs_exact(rows, gnrs_key(cid[rows], cpv[rows]), alt_keys[ord], alt_ids[ord])
  gnrs_set_state(u, hit$rows, hit$ids, "exact standard name, county-as-state", clear_county = TRUE)
}

gnrs_step_countyasstate_fuzzy <- function(u, ctx) {
  cpv <- u$county_parish_verbatim
  todo <- function() which(gnrs_is_sac_row(u, ctx) & is.na(u$state_province_id) & !gnrs_blank(cpv))

  for (i in todo()) {
    if (!gnrs_wildcard_ok(cpv[i])) next
    id <- gnrs_wildcard_unique(ctx, cpv[i], "state_alt", u$country_id[i])
    if (!is.na(id)) {
      u <- gnrs_set_state(u, i, id, "wildcard alternate name, county-as-state", clear_county = TRUE)
    }
  }
  for (what in c("state_name", "state_ascii", "state_std")) {
    for (i in todo()) {
      best <- gnrs_fuzzy_best(ctx, cpv[i], what, u$country_id[i])
      if (!is.null(best)) {
        u <- gnrs_set_state(u, i, best$id, "fuzzy standard name, county-as-state",
          score = best$score, clear_county = TRUE
        )
      }
    }
  }
  u
}

# ---------------------------------------------------------------------------
# Summary: lowest division submitted and matched, match status, codes,
# identifiers and scores.  Ports summarize.sql, iso_codes.sql, gadm_ids.sql,
# geonames_ids.sql and update_match_scores.sql.
# ---------------------------------------------------------------------------

gnrs_summarize <- function(u, bb) {
  co <- bb$country
  st <- bb$state
  ct <- bb$county
  cv <- u$country_verbatim
  sv <- u$state_province_verbatim
  cpv <- u$county_parish_verbatim
  has <- function(method, what) !is.na(method) & grepl(what, method, fixed = TRUE)

  submitted <- rep(NA_character_, nrow(u))
  submitted[!gnrs_blank(cv)] <- "country"
  submitted[!gnrs_blank(sv)] <- "state_province"
  submitted[!gnrs_blank(cpv)] <- "county_parish"

  matched <- rep(NA_character_, nrow(u))
  matched[!is.na(u$country) & is.na(u$state_province) & is.na(u$county_parish)] <- "country"
  matched[!is.na(u$country) & !is.na(u$state_province) & is.na(u$county_parish)] <- "state_province"
  matched[!is.na(u$country) & !is.na(u$state_province) & !is.na(u$county_parish)] <- "county_parish"
  matched[has(u$match_method_country, "state-as-country")] <- "state-as-country"
  matched[has(u$match_method_state_province, "county-as-state")] <- "county-as-state"
  matched[has(u$match_method_state_province, "country-as-state")] <- "country-as-state"
  matched[has(u$match_method_county_parish, "state-as-county")] <- "state-as-county"

  status <- rep(NA_character_, nrow(u))
  both <- !is.na(submitted) & !is.na(matched)
  status[both & submitted == matched] <- "full match"
  status[both & submitted != matched] <- "partial match"
  status[!is.na(submitted) & is.na(matched)] <- "no match"
  i <- !is.na(matched) & matched == "state-as-country"
  status[i] <- ifelse(gnrs_blank(cpv[i]), "full match", "partial match")
  i <- !is.na(matched) & matched == "county-as-state"
  status[i] <- "full match"
  i <- !is.na(matched) & matched == "country-as-state"
  status[i] <- ifelse(gnrs_blank(sv[i]), "full match", "partial match")
  i <- !is.na(matched) & matched == "state-as-county"
  status[i] <- "full match"

  u$poldiv_submitted <- submitted
  u$poldiv_matched <- matched
  u$match_status <- status

  cm <- match(u$country_id, co$country_id)
  sm <- match(u$state_province_id, st$state_province_id)
  km <- match(u$county_parish_id, ct$county_parish_id)
  u$country_iso <- co$iso[cm]
  u$state_province_iso <- st$state_province_code[sm]
  u$county_parish_iso <- ct$county_parish_code[km]
  u$gid_0 <- co$gid_0[cm]
  u$gid_1 <- st$gid_1[sm]
  u$gid_2 <- ct$gid_2[km]

  # The identifier of the lowest division matched.  The upstream script
  # intends to restrict this to GeoNames divisions, but the deployed service
  # reports it for every division, so this does too.
  geonameid <- u$country_id
  geonameid[!is.na(u$state_province_id)] <- u$state_province_id[!is.na(u$state_province_id)]
  geonameid[!is.na(u$county_parish_id)] <- u$county_parish_id[!is.na(u$county_parish_id)]
  u$geonameid <- geonameid

  # Scores.  A division matched through one of the cross-level routes is
  # scored by the similarity of what was submitted at the other level to the
  # name it resolved to; any other matched division scores 1, an unmatched
  # one 0, and a division that was not submitted stays unscored.
  pair <- function(a, b) {
    vapply(seq_along(a), function(j) gnrs_numeric2(gnrs_similarity_pair(a[j], b[j])), numeric(1))
  }
  sc <- u$match_score_country
  i <- is.na(sc) & !gnrs_blank(cv) & !is.na(u$country) & has(u$match_method_country, "state-as-country")
  if (any(i)) sc[i] <- pair(sv[i], u$country[i])
  i <- is.na(sc) & !gnrs_blank(cv)
  sc[i] <- ifelse(is.na(u$country[i]) | !nzchar(u$country[i]), 0, 1)

  ss <- u$match_score_state_province
  i <- is.na(ss) & !gnrs_blank(cv) & !is.na(u$state_province) & has(u$match_method_state_province, "country-as-state")
  if (any(i)) ss[i] <- pair(cv[i], u$state_province[i])
  i <- is.na(ss) & !gnrs_blank(sv)
  ss[i] <- ifelse(is.na(u$state_province[i]) | !nzchar(u$state_province[i]), 0, 1)

  sk <- u$match_score_county_parish
  i <- is.na(sk) & !gnrs_blank(sv) & !is.na(u$county_parish) & has(u$match_method_county_parish, "state-as-county")
  if (any(i)) sk[i] <- pair(sv[i], u$county_parish[i])
  i <- is.na(sk) & !gnrs_blank(cpv)
  sk[i] <- ifelse(is.na(u$county_parish[i]) | !nzchar(u$county_parish[i]), 0, 1)

  overall <- rep(NA_real_, nrow(u))
  ok <- !is.na(sc)
  i <- ok & is.na(ss) & is.na(sk)
  overall[i] <- sc[i]
  i <- ok & !is.na(ss) & is.na(sk)
  overall[i] <- (sc[i] + ss[i]) / 2
  i <- ok & !is.na(ss) & !is.na(sk)
  overall[i] <- (sc[i] + ss[i] + sk[i]) / 3

  u$match_score_country <- gnrs_numeric2(sc)
  u$match_score_state_province <- gnrs_numeric2(ss)
  u$match_score_county_parish <- gnrs_numeric2(sk)
  u$overall_score <- gnrs_numeric2(overall)
  u
}
