#' Strip administrative-class words from submitted names
#'
#' Internal.  R port of \code{sql/populate_sp_verbatim_alt.sql} and
#' \code{sql/populate_cp_verbatim_alt.sql}.  Before matching, the service
#' derives from each submitted state and county name an alternate form with
#' words such as "Province", "Departamento de" or "County" removed, and one of
#' the wildcard steps later matches on that form.
#'
#' The SQL is three CASE statements applied in turn.  The first, prefixes that
#' include an article, sets the alternate form or leaves it NULL; the second
#' (prefixes without an article) and third (suffixes) only fill in rows the
#' earlier ones left empty.  Within each, the first test that fires wins, and
#' its replacement removes the first occurrence of its text, case-insensitively.
#' The result is trimmed, and a short list of names that the rules mangle is
#' restored afterwards.  The rule tables are transcribed mechanically from the
#' SQL; see \code{local_alt_rules.R}.
#'
#' @param x Character vector of submitted names.
#' @param rules From \code{gnrs_state_alt_rules()} or \code{gnrs_county_alt_rules()}.
#' @param restore Character vector of exact values whose alternate form is
#'   reset to the value itself.
#' @param restore_prefix Character vector of prefixes (SQL LIKE 'x\%') with the
#'   same effect.
#' @param restore_to Named character vector: a value whose alternate form is
#'   set to something else.
#' @return Character vector of the same length: the alternate form, or "" where
#'   no rule applied.
#' @keywords internal
#' @noRd
gnrs_verbatim_alt <- function(x, rules, restore = character(0),
                              restore_prefix = character(0),
                              restore_to = character(0)) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  alt <- rep("", length(x))
  has <- nzchar(x)

  for (group in rules) {
    todo <- has & !nzchar(alt)
    if (!any(todo)) {
      next
    }
    alt[todo] <- gnrs_apply_alt_rules(x[todo], group)
  }

  alt <- trimws(alt)

  # The restorations apply whatever state the alternate form is in, as in the
  # SQL, which has no guard on them
  for (value in restore) {
    alt[x == value] <- value
  }
  for (prefix in restore_prefix) {
    alt[startsWith(x, prefix)] <- x[startsWith(x, prefix)]
  }
  for (value in names(restore_to)) {
    alt[x == value] <- restore_to[[value]]
  }

  alt
}

#' Apply one CASE block of stripping rules
#'
#' Internal.  Returns the alternate form for each value, or "" where no test
#' fired (the SQL's ELSE NULL).
#' @keywords internal
#' @noRd
gnrs_apply_alt_rules <- function(x, group) {
  out <- rep("", length(x))
  pending <- rep(TRUE, length(x))
  for (i in seq_len(nrow(group))) {
    if (!any(pending)) {
      break
    }
    hit <- pending & gnrs_ilike(x, group$like[i])
    if (nzchar(group$not_like[i])) {
      hit <- hit & !gnrs_like(x, group$not_like[i])
    }
    if (any(hit)) {
      # regexp_replace(x, text, '', 'i'): first occurrence, case-insensitive.
      # The text is literal, so it is matched fixed rather than as a pattern;
      # a rule whose text does not occur (upstream uses 'xxx' as a no-op)
      # simply returns the value unchanged, which is what the CASE does too.
      out[hit] <- gnrs_remove_first_ci(x[hit], group$replace[i])
      pending <- pending & !hit
    }
  }
  out
}

#' SQL LIKE and ILIKE against one pattern
#'
#' Internal.  Only the percent and underscore wildcards are needed; the
#' patterns are fixed text from the rule tables.
#' @keywords internal
#' @noRd
gnrs_like_regex <- function(pattern) {
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", pattern)
  escaped <- gsub("%", ".*", escaped, fixed = TRUE)
  escaped <- gsub("_", ".", escaped, fixed = TRUE)
  paste0("^", escaped, "$")
}

#' @keywords internal
#' @noRd
gnrs_ilike <- function(x, pattern) {
  grepl(gnrs_like_regex(pattern), x, ignore.case = TRUE, perl = TRUE)
}

#' @keywords internal
#' @noRd
gnrs_like <- function(x, pattern) {
  grepl(gnrs_like_regex(pattern), x, perl = TRUE)
}

#' Remove the first occurrence of literal text, ignoring case
#' @keywords internal
#' @noRd
gnrs_remove_first_ci <- function(x, text) {
  if (!nzchar(text)) {
    return(x)
  }
  # Located on lower-cased copies, since a fixed search cannot ignore case;
  # positions carry over because lower-casing keeps the character count
  at <- regexpr(gnrs_lower(text), gnrs_lower(x), fixed = TRUE)
  found <- at > 0
  if (!any(found)) {
    return(x)
  }
  len <- attr(at, "match.length")
  x[found] <- paste0(
    substr(x[found], 1L, at[found] - 1L),
    substr(x[found], at[found] + len[found], nchar(x[found]))
  )
  x
}

#' Alternate form of submitted state/province names
#'
#' Internal.  See \code{gnrs_verbatim_alt()}.  The restored values are the
#' "Restore mistakes" block of the upstream SQL, some of which carry a trailing
#' space in the original and do here too.
#' @keywords internal
#' @noRd
gnrs_state_verbatim_alt <- function(x) {
  gnrs_verbatim_alt(
    x, gnrs_state_alt_rules(),
    restore = c(
      "Al Batinah North Governorate", "Al Batinah South Governorate",
      "Al Bayda Governorate", "Central District", "Central Division",
      "Central Province ", "Central Region", "Centre Region",
      "Departamento Central", "Departement du Nord-Est", "Eastern District",
      "Eastern Division", "Eastern Province", "Eastern Region",
      "Far North Region", "Federal Capital Territory", "Federal District",
      "Midway Islands", "National Capital District", "National Capital Region",
      "North Central Province ", "North East District", "Northern District",
      "Northern Division", "Northern Governorate", "Northern Province",
      "Northern Region", "Northern Territory", "North Region",
      "North West District", "North Western Province",
      "North-Western Province", "North-West Region", "North Central Province",
      "West Region", "Western Region", "Western Province",
      "Southern Nations, Nationalities, and People's Region",
      "Red Sea Governorate"
    )
  )
}

#' Alternate form of submitted county/parish names
#' @keywords internal
#' @noRd
gnrs_county_verbatim_alt <- function(x) {
  gnrs_verbatim_alt(
    x, gnrs_county_alt_rules(),
    restore = c(
      "Quan 12", "San Cristobal De Casas", "San Sebastian del Oeste",
      "Santa Barbara D'Oeste", "Santa Barbara Do Sul", "Santa Fe Do Sul",
      "Santa Izabel Do Oeste", "Santa Lucia del Camino", "Santa Luzia Do Norte",
      "Santa Luzia D'Oeste", "Santa Maria Da Serra", "Santa Maria Do Oeste",
      "Santa Maria del Rio", "Santa Maria del Real", "Santa Rosa Do Sul",
      "Santa Tereza Do Oeste", "Santiago Do Sul", "Santo Antonio Do Leste",
      "Santo Antonio Do Sudoeste", "Santo Domingo Este", "Santo Domingo Norte",
      "Santo Domingo Oeste", "Sao Bento Do Sul", "Sao Bento do Norte",
      "Sao Domingos Do Norte", "Sao Domingos Do Sul", "Sao Francisco Do Oeste",
      "Sao Francisco Do Sul", "Sao Gabriel Do Oeste", "Sao Joao Do Oeste",
      "Sao Joao Do Oriente", "Sao Joao Do Sul", "Sao Lourenco do Oeste",
      "Sao Lourenco Do Sul", "Sao Pedro Do Sul", "Sao Pedro do Sul",
      "Sao Sebastiao Do Oeste", "Santa Barbara Do Leste"
    ),
    restore_prefix = paste0("Comuna ", 1:9),
    restore_to = c("San Jeronimo Department" = "San Jeronimo")
  )
}
