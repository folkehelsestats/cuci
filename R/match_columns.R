# =============================================================================
# match_columns.R
#
# Three-layer column name matching pipeline.
#
# Public API (exported):
#   match_columns()          - orchestrator; runs all three layers in order
#   print_match_report()     - console summary of matching decisions
#   build_keyword_patterns() - compile regex patterns from YAML keywords
#
# Internal helpers (not exported, prefixed with `.`):
#   .build_match_row()       - construct one log data.table row
#   .match_exact()           - layer 1: lookup against known aliases
#   .match_fuzzy()           - layer 2: Levenshtein edit-distance via agrep()
#   .find_matched_keyword()  - identify which plain keyword triggered a match
#   .match_keyword()         - layer 3: regex pattern search
#   .compile_match_result()  - assemble rename_vec and unmatched from the log
# =============================================================================


# -----------------------------------------------------------------------------
# .build_match_row()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Build a single-row data.table for the match log
#'
#' Centralises the repeated data.table construction that would otherwise be
#' copy-pasted across the three match layers. Every layer calls this so the
#' log schema is guaranteed to be identical everywhere.
#'
#' @param raw_name     The column name as it appears in the raw dataset.
#' @param canonical    The canonical variable name it maps to.
#' @param method       Free-text method label ("exact", "fuzzy", or
#'                     "keyword [word]").
#' @param confidence   One of "high", "medium", or "low".
#' @param needs_review Logical - should a human verify this decision?
#'
#' @return A one-row data.table.
.build_match_row <- function(raw_name, canonical, method, confidence, needs_review) {
  data.table::data.table(
    raw_name     = raw_name,
    canonical    = canonical,
    method       = method,
    confidence   = confidence,
    needs_review = needs_review
  )
}


# -----------------------------------------------------------------------------
# .match_exact()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Layer 1 - exact alias lookup
#'
#' Checks each candidate column name directly against `config$name_lookup`,
#' a named vector where keys are known aliases (raw and normalised) and values
#' are canonical names.  Only the first unclaimed canonical wins.
#'
#' @param candidates       Character vector of column names not yet matched.
#' @param name_lookup      Named character vector from `config$name_lookup`.
#' @param claimed_canonical Character vector of canonical names already taken.
#'
#' @return A list: `rows` (list of one-row data.tables), `claimed` (updated
#'   claimed_canonical), `remaining` (column names that did not exact-match).
.match_exact <- function(candidates, name_lookup, claimed_canonical) {

  rows <- list()

  for (col in candidates) {
    if (!col %in% names(name_lookup)) next

    canonical <- name_lookup[[col]]
    if (canonical %in% claimed_canonical) next

    rows[[length(rows) + 1]] <- .build_match_row(
      raw_name     = col,
      canonical    = canonical,
      method       = "exact",
      confidence   = "high",
      needs_review = FALSE
    )
    claimed_canonical <- c(claimed_canonical, canonical)
  }

  matched_raw <- vapply(rows, `[[`, character(1), "raw_name")

  list(
    rows      = rows,
    claimed   = claimed_canonical,
    remaining = setdiff(candidates, matched_raw)
  )
}


# -----------------------------------------------------------------------------
# .match_fuzzy()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Layer 2 - fuzzy alias matching via Levenshtein edit distance
#'
#' Uses [base::agrep()] to find aliases within `max_distance` edit-distance
#' of each candidate.  Takes the first hit (closest distance).  Columns that
#' already have an exact match are not passed in, so no overlap can occur.
#'
#' @param candidates       Character vector of unmatched column names.
#' @param name_lookup      Named character vector from `config$name_lookup`.
#' @param claimed_canonical Character vector of canonical names already taken.
#' @param max_distance     Passed to `agrep(max.distance=)`. Default 0.15.
#'
#' @return Same structure as [.match_exact()].
.match_fuzzy <- function(candidates, name_lookup, claimed_canonical,
                         max_distance = 0.15) {

  rows        <- list()
  all_aliases <- names(name_lookup)

  for (col in candidates) {
    hits <- agrep(
      pattern      = col,
      x            = all_aliases,
      max.distance = max_distance,
      value        = TRUE,
      ignore.case  = TRUE
    )

    if (length(hits) == 0) next

    canonical <- name_lookup[[ hits[1] ]]
    if (canonical %in% claimed_canonical) next

    rows[[length(rows) + 1]] <- .build_match_row(
      raw_name     = col,
      canonical    = canonical,
      method       = "fuzzy",
      confidence   = "medium",
      needs_review = TRUE
    )
    claimed_canonical <- c(claimed_canonical, canonical)
  }

  matched_raw <- vapply(rows, `[[`, character(1), "raw_name")

  list(
    rows      = rows,
    claimed   = claimed_canonical,
    remaining = setdiff(candidates, matched_raw)
  )
}


# -----------------------------------------------------------------------------
# .find_matched_keyword()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Identify which plain keyword caused a regex hit
#'
#' After [.match_keyword()] confirms that a pattern matched, this function
#' looks up the *plain* keyword list from `var_map` (not the compiled regex)
#' and tests each keyword individually with `fixed = TRUE`.  This avoids
#' splitting or re-parsing lookahead/lookbehind patterns for display purposes.
#'
#' Returns the longest matching keyword, or `"(pattern)"` if none of the
#' individual keywords can be isolated (should not happen in practice).
#'
#' @param col       The raw column name that was matched.
#' @param canonical The canonical variable name whose pattern fired.
#' @param var_map   The raw parsed YAML list (`config$var_map`).
#'
#' @return A single string: the keyword that matched.
.find_matched_keyword <- function(col, canonical, var_map) {

  plain_kws <- as.character(var_map[[canonical]]$keywords)
  plain_kws <- plain_kws[!is.na(plain_kws) & nchar(plain_kws) >= 3]

  if (length(plain_kws) == 0) return("(pattern)")

  hit_flags <- vapply(
    plain_kws,
    function(kw) grepl(tolower(kw), tolower(col), fixed = TRUE),
    logical(1)
  )

  matching <- plain_kws[hit_flags]

  if (length(matching) == 0) "(pattern)" else matching[which.max(nchar(matching))]
}


# -----------------------------------------------------------------------------
# .match_keyword()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Layer 3 - keyword / regex pattern matching
#'
#' Iterates over every variable's compiled regex pattern and tests each
#' candidate column name.  The first unclaimed canonical whose pattern fires
#' wins.  Calls [.find_matched_keyword()] to produce a human-readable label
#' for the match log.
#'
#' @param candidates        Character vector of unmatched column names.
#' @param keyword_patterns  Named list of regex strings from `config$keyword_patterns`.
#' @param claimed_canonical Character vector of canonical names already taken.
#' @param var_map           Raw YAML list for keyword display lookup.
#'
#' @return Same structure as [.match_exact()].
.match_keyword <- function(candidates, keyword_patterns, claimed_canonical, var_map) {

  rows <- list()

  for (col in candidates) {

    matched_canonical <- NULL
    matched_kw_label  <- NULL

    for (canonical in names(keyword_patterns)) {
      if (canonical %in% claimed_canonical) next

      pattern <- keyword_patterns[[canonical]]
      if (is.null(pattern) || identical(pattern, "") || is.na(pattern)) next

      if (!grepl(pattern, col, ignore.case = TRUE, perl = TRUE)) next

      matched_canonical <- canonical
      matched_kw_label  <- .find_matched_keyword(col, canonical, var_map)
      break
    }

    if (is.null(matched_canonical)) next

    rows[[length(rows) + 1]] <- .build_match_row(
      raw_name     = col,
      canonical    = matched_canonical,
      method       = sprintf("keyword [%s]", matched_kw_label),
      confidence   = "low",
      needs_review = TRUE
    )
    claimed_canonical <- c(claimed_canonical, matched_canonical)
  }

  matched_raw <- vapply(rows, `[[`, character(1), "raw_name")

  list(
    rows      = rows,
    claimed   = claimed_canonical,
    remaining = setdiff(candidates, matched_raw)
  )
}


# -----------------------------------------------------------------------------
# .compile_match_result()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Compile the final match result from accumulated log rows
#'
#' Takes the flat list of one-row data.tables produced by all three match
#' layers plus the original column names, and assembles the three-element
#' list that [match_columns()] returns.
#'
#' Extracting this as a helper keeps [match_columns()] free of bookkeeping
#' logic and makes it trivial to unit-test result assembly independently.
#'
#' @param row_list      List of one-row data.tables from all layers.
#' @param raw_colnames  The original full set of column names.
#'
#' @return List with `match_log`, `rename_vec`, and `unmatched`.
.compile_match_result <- function(row_list, raw_colnames) {

  if (length(row_list) == 0) {
    return(list(
      match_log  = data.table::data.table(),
      rename_vec = setNames(character(0), character(0)),
      unmatched  = raw_colnames
    ))
  }

  log_dt <- data.table::rbindlist(row_list)

  # Only rows where the name actually changed need to be in rename_vec
  rename_rows <- log_dt[log_dt$raw_name != log_dt$canonical, ]
  rename_vec  <- setNames(rename_rows$canonical, rename_rows$raw_name)

  list(
    match_log  = log_dt,
    rename_vec = rename_vec,
    unmatched  = setdiff(raw_colnames, log_dt$raw_name)
  )
}


# -----------------------------------------------------------------------------
# match_columns()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Match raw column names to canonical variable names
#'
#' Thin orchestrator: passes column names through three matching layers in
#' descending confidence order (exact → fuzzy → keyword) and returns a
#' structured result.  Each layer is implemented as a separate internal helper
#' so individual layers can be tested or extended independently.
#'
#' @param raw_colnames      Character vector of column names from the raw
#'   dataset (after name normalisation).
#' @param config            Config object returned by [load_config()].
#' @param fuzzy_max_distance Numeric. Maximum edit distance for fuzzy matching.
#'   Default `0.15`.
#'
#' @return A named list:
#' \describe{
#'   \item{match_log}{`data.table` with one row per matched column.}
#'   \item{rename_vec}{Named character vector `old -> canonical` for
#'     [data.table::setnames()].}
#'   \item{unmatched}{Column names that fell through all three layers.}
#' }
#'
#' @examples
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#' cfg <- load_config(yml)
#' match_columns(c("kjonn", "alder", "unknown_col"), cfg)$match_log
#'
#' @importFrom data.table data.table rbindlist
#' @export
match_columns <- function(raw_colnames, config, fuzzy_max_distance = 0.15) {

  if (length(config$name_lookup) == 0) {
    warning(
      "config$name_lookup is empty - exact and fuzzy matching will be skipped.\n",
      "  Verify that load_config() reads the 'colnames:' field from your YAML.",
      call. = FALSE
    )
  }

  all_rows        <- list()
  claimed         <- character(0)

  # --- Layer 1: exact ---------------------------------------------------
  exact  <- .match_exact(raw_colnames, config$name_lookup, claimed)
  all_rows <- c(all_rows, exact$rows)
  claimed  <- exact$claimed

  # --- Layer 2: fuzzy ---------------------------------------------------
  fuzzy  <- .match_fuzzy(exact$remaining, config$name_lookup, claimed,
                         max_distance = fuzzy_max_distance)
  all_rows <- c(all_rows, fuzzy$rows)
  claimed  <- fuzzy$claimed

  # --- Layer 3: keyword -------------------------------------------------
  kw     <- .match_keyword(fuzzy$remaining, config$keyword_patterns,
                           claimed, config$var_map)
  all_rows <- c(all_rows, kw$rows)

  # --- Compile ----------------------------------------------------------
  .compile_match_result(all_rows, raw_colnames)
}


# -----------------------------------------------------------------------------
# print_match_report()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Print a human-readable column matching report
#'
#' Summarises the result of [match_columns()] to the console, grouped by
#' confidence tier.
#'
#' @param match_result List returned by [match_columns()].
#' @param dataset_label Optional label for the heading.
#'
#' @return `NULL`, invisibly.
#' @export
print_match_report <- function(match_result, dataset_label = "") {

  log <- match_result$match_log
  if ((is.null(log) || nrow(log) == 0) && length(match_result$unmatched) == 0) {
    return(invisible())
  }

  charLab <- sprintf("  Column Matching Report: %s\n", dataset_label)
  nlab <- nchar(charLab) + 2
  
  cat(sprintf("\n\u2554%s\u2557\n", strrep("\u2550", nlab)))
  cat(charLab)
  cat(sprintf("\u255a%s\u255d\n", strrep("\u2550", nlab)))

  exact <- log[log$method == "exact", ]
  if (nrow(exact) > 0) {
    cat(sprintf("\n  \u2714 EXACT MATCHES (%d):\n", nrow(exact)))
    for (i in seq_len(nrow(exact)))
      cat(sprintf("    %-25s -> %s\n", exact$raw_name[i], exact$canonical[i]))
  }

  fuzzy <- log[log$method == "fuzzy", ]
  if (nrow(fuzzy) > 0) {
    cat(sprintf("\n  \u26a0 FUZZY MATCHES \u2014 please double-check (%d):\n", nrow(fuzzy)))
    for (i in seq_len(nrow(fuzzy)))
      cat(sprintf("    %-25s -> %-15s  [possible typo/variant]\n",
                  fuzzy$raw_name[i], fuzzy$canonical[i]))
  }

  keyword <- log[grepl("^keyword", log$method), ]
  if (nrow(keyword) > 0) {
    cat(sprintf("\n  \U0001f6ab KEYWORD/PREDICTED MATCHES \u2014 must verify (%d):\n", nrow(keyword)))
    for (i in seq_len(nrow(keyword)))
      cat(sprintf("    %-25s -> %-15s  matched by %s\n",
                  keyword$raw_name[i], keyword$canonical[i], keyword$method[i]))
  }

  if (length(match_result$unmatched) > 0) {
    cat(sprintf("\n  \u2716 UNMATCHED COLUMNS \u2014 not included in output (%d):\n",
                length(match_result$unmatched)))

    # Split into groups of 5
    chunks <- split(
      match_result$unmatched,
      ceiling(seq_along(match_result$unmatched) / 5)
    )

    # Print one line per chunk
    cat(sprintf("    %s\n", sapply(chunks, paste, collapse = ", ")))
    
    # cat(sprintf("    %s\n", paste(match_result$unmatched, collapse = ", ")))
    cat("    \u2192 Consider adding these to your variable_map.yml\n")
  }

  cat("\n")
  invisible()
}


# -----------------------------------------------------------------------------
# build_keyword_patterns()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Build regex keyword patterns from a variable map config
#'
#' For each variable with a `keywords:` block, produces a single regex that
#' matches any of those keywords as a substring not immediately surrounded by
#' other alphanumeric characters (so `kjonn` matches `io_kjonn` but not
#' `kjonnstudie`).
#'
#' @param config   List with a `$var_map` element (from [load_config()]).
#' @param min_char Minimum keyword length to include. Default `3`.
#'
#' @return Named list of regex strings; variables without usable keywords are
#'   omitted.
#' @export
build_keyword_patterns <- function(config, min_char = 3) {

  patterns <- lapply(config$var_map, function(var_def) {

    kws <- var_def$keywords
    if (is.null(kws) || all(is.na(kws))) return(NULL)

    kws <- as.character(kws)
    kws <- tolower(trimws(kws))
    kws <- kws[!kws %in% c("", "na", "null", "none") & !is.na(kws)]
    kws <- kws[nchar(kws) >= min_char]
    if (length(kws) == 0) return(NULL)

    # Escape regex metacharacters in keyword strings
    kws <- gsub("([.^$*+?()\\[{\\\\|])", "\\\\\\1", kws)

    # Lookahead/lookbehind instead of \b: underscore is a \w character so
    # \b would NOT fire between "_" and "k" in "io_kjonn".
    paste0("(?<![a-z0-9])(", paste(unique(kws), collapse = "|"), ")(?![a-z0-9])")
  })

  Filter(Negate(is.null), patterns)
}
