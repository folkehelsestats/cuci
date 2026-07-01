#' Match raw column names to canonical variable names
#'
#' Attempts to map every raw column name to a canonical name defined in the
#' variable map using three layers, in order of confidence:
#'
#' \enumerate{
#'   \item **Exact match** – raw name is a known alias (highest confidence).
#'   \item **Fuzzy match** – raw name is within edit-distance of a known alias.
#'   \item **Keyword match** – raw name contains a known keyword (lowest confidence).
#' }
#'
#' @param raw_colnames Character vector of column names from the raw dataset
#'   (after `janitor::clean_names()` normalisation).
#' @param config Config object returned by [load_config()].
#' @param fuzzy_max_distance Numeric. Maximum edit distance for fuzzy matching
#'   (passed to [base::agrep()]). Default `0.15` (~15% of chars may differ).
#'
#' @return A named list:
#' \describe{
#'   \item{match_log}{`data.table` with one row per matched column.}
#'   \item{rename_vec}{Named character vector `old_name -> canonical_name`,
#'     ready for [data.table::setnames()].}
#'   \item{unmatched}{Character vector of columns that could not be mapped.}
#' }
#'
#' @examples
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#' config <- load_config(yml)
#' result <- match_columns(c("Kjonn", "Alder", "unknown_col"), config)
#' result$match_log
#'
#' @importFrom data.table data.table rbindlist
#' @export
match_columns <- function(raw_colnames, config, fuzzy_max_distance = 0.15) {

  canonical_vars <- names(config$var_map)

  if (length(config$name_lookup) == 0) {
    warning(
      "config$name_lookup is empty — exact and fuzzy matching will be skipped.\n",
      "  Verify that load_config() reads the 'colnames:' field from your YAML.",
      call. = FALSE
    )
  }

  unmatched        <- raw_colnames
  match_log        <- list()
  claimed_canonical <- character(0)


  # ================================================================
  # LAYER 1: EXACT MATCH
  # ================================================================
  for (col in unmatched) {
    if (col %in% names(config$name_lookup)) {
      canonical <- config$name_lookup[[col]]
      if (canonical %in% claimed_canonical) next

      match_log[[length(match_log) + 1]] <- data.table::data.table(
        raw_name     = col,
        canonical    = canonical,
        method       = "exact",
        confidence   = "high",
        needs_review = FALSE
      )
      claimed_canonical <- c(claimed_canonical, canonical)
    }
  }

  exact_matched <- vapply(match_log, `[[`, character(1), "raw_name")
  unmatched     <- setdiff(unmatched, exact_matched)


  # ================================================================
  # LAYER 2: FUZZY MATCH (Levenshtein edit distance)
  # ================================================================
  all_aliases <- names(config$name_lookup)

  for (col in unmatched) {
    fuzzy_hits <- agrep(
      pattern      = col,
      x            = all_aliases,
      max.distance = fuzzy_max_distance,
      value        = TRUE,
      ignore.case  = TRUE
    )

    if (length(fuzzy_hits) == 0) next

    best_alias <- fuzzy_hits[1]
    canonical  <- config$name_lookup[[best_alias]]
    if (canonical %in% claimed_canonical) next

    match_log[[length(match_log) + 1]] <- data.table::data.table(
      raw_name     = col,
      canonical    = canonical,
      method       = "fuzzy",
      confidence   = "medium",
      needs_review = TRUE
    )
    claimed_canonical <- c(claimed_canonical, canonical)
  }

  fuzzy_matched <- vapply(
    Filter(function(x) x$method == "fuzzy", match_log),
    `[[`, character(1), "raw_name"
  )
  unmatched <- setdiff(unmatched, fuzzy_matched)


  # ================================================================
  # LAYER 3: KEYWORD / PATTERN MATCH
  # ================================================================
  for (col in unmatched) {
    matched_canonical <- NULL
    matched_keyword   <- NULL

    for (canonical in names(config$keyword_patterns)) {
      if (canonical %in% claimed_canonical) next

      pattern <- config$keyword_patterns[[canonical]]
      if (is.null(pattern) || identical(pattern, "") || identical(pattern, NA_character_)) next

      if (grepl(pattern, col, ignore.case = TRUE, perl = TRUE)) {
        matched_canonical <- canonical

        # Use plain keywords from the var_map for display (not the regex string)
        # so we avoid parsing lookahead/lookbehind fragments.
        plain_kws <- as.character(config$var_map[[canonical]]$keywords)
        plain_kws <- plain_kws[!is.na(plain_kws) & nchar(plain_kws) >= 3]

        hit_flags <- vapply(
          plain_kws,
          function(kw) grepl(kw, col, ignore.case = TRUE, fixed = TRUE),
          logical(1)
        )

        matching_kws    <- plain_kws[hit_flags]
        matched_keyword <- if (length(matching_kws) == 0) {
          "(pattern)"
        } else {
          matching_kws[which.max(nchar(matching_kws))]
        }

        break
      }
    }

    if (!is.null(matched_canonical)) {
      match_log[[length(match_log) + 1]] <- data.table::data.table(
        raw_name     = col,
        canonical    = matched_canonical,
        method       = sprintf('keyword ["%s"]', matched_keyword),
        confidence   = "low",
        needs_review = TRUE
      )
      claimed_canonical <- c(claimed_canonical, matched_canonical)
    }
  }


  # ================================================================
  # Compile results
  # ================================================================
  if (length(match_log) == 0) {
    return(list(
      match_log  = data.table::data.table(),
      rename_vec = setNames(character(0), character(0)),
      unmatched  = raw_colnames
    ))
  }

  match_log_dt <- data.table::rbindlist(match_log)

  rename_vec <- with(
    match_log_dt[match_log_dt$raw_name != match_log_dt$canonical, ],
    setNames(canonical, raw_name)
  )

  all_matched     <- match_log_dt$raw_name
  truly_unmatched <- setdiff(raw_colnames, all_matched)

  list(
    match_log  = match_log_dt,
    rename_vec = rename_vec,
    unmatched  = truly_unmatched
  )
}


#' Print a human-readable column matching report
#'
#' Summarises the result of [match_columns()] to the console, grouping
#' matches by confidence tier and highlighting columns that need review.
#'
#' @param match_result List returned by [match_columns()].
#' @param dataset_label Optional string label for the dataset (used in heading).
#'
#' @return Invisibly returns `NULL`. Called for its side-effect (printing).
#'
#' @export
print_match_report <- function(match_result, dataset_label = "") {

  log <- match_result$match_log
  if ((is.null(log) || nrow(log) == 0) && length(match_result$unmatched) == 0) {
    return(invisible())
  }

  cat(sprintf("\n\u2554%s\u2557\n", strrep("\u2550", 54)))
  cat(sprintf("  Column Matching Report: %s\n", dataset_label))
  cat(sprintf("\u255a%s\u255d\n", strrep("\u2550", 54)))

  exact <- log[log$method == "exact", ]
  if (nrow(exact) > 0) {
    cat(sprintf("\n  \u2714 EXACT MATCHES (%d):\n", nrow(exact)))
    for (i in seq_len(nrow(exact))) {
      cat(sprintf("    %-25s -> %s\n", exact$raw_name[i], exact$canonical[i]))
    }
  }

  fuzzy <- log[log$method == "fuzzy", ]
  if (nrow(fuzzy) > 0) {
    cat(sprintf("\n  \u26a0 FUZZY MATCHES \u2014 please double-check (%d):\n", nrow(fuzzy)))
    for (i in seq_len(nrow(fuzzy))) {
      cat(sprintf("    %-25s -> %-15s  [possible typo/variant]\n",
                  fuzzy$raw_name[i], fuzzy$canonical[i]))
    }
  }

  keyword <- log[grepl("^keyword", log$method), ]
  if (nrow(keyword) > 0) {
    cat(sprintf("\n  \U0001f6ab KEYWORD/PREDICTED MATCHES \u2014 must verify (%d):\n", nrow(keyword)))
    for (i in seq_len(nrow(keyword))) {
      cat(sprintf("    %-25s -> %-15s  matched by %s\n",
                  keyword$raw_name[i], keyword$canonical[i], keyword$method[i]))
    }
  }

  if (length(match_result$unmatched) > 0) {
    cat(sprintf("\n  \u2716 UNMATCHED COLUMNS \u2014 not included in output (%d):\n",
                length(match_result$unmatched)))
    cat(sprintf("    %s\n", paste(match_result$unmatched, collapse = ", ")))
    cat("    \u2192 Consider adding these to your variable_map.yml\n")
  }

  cat("\n")
  invisible()
}


#' Build regex keyword patterns from a variable map config
#'
#' Constructs a strict word-boundary regex for each variable's `keywords`
#' block, suitable for use in [match_columns()].
#'
#' @param config A list with a `$var_map` element (as returned by [load_config()]).
#' @param min_char Minimum character length for a keyword to be included.
#'   Default `3`.
#'
#' @return Named list of regex strings, one per variable that has keywords.
#'   Variables with no usable keywords are omitted.
#'
#' @export
build_keyword_patterns <- function(config, min_char = 3) {

  keyword_patterns <- lapply(config$var_map, function(var_def) {

    keywords <- var_def$keywords
    if (is.null(keywords) || all(is.na(keywords))) return(NULL)

    keywords <- as.character(keywords)
    keywords <- tolower(trimws(keywords))
    keywords <- keywords[!keywords %in% c("", "na", "null", "none") & !is.na(keywords)]
    keywords <- keywords[nchar(keywords) >= min_char]

    if (length(keywords) == 0) return(NULL)

    # Escape regex special characters
    keywords <- gsub("([.^$*+?()\\[{\\\\|])", "\\\\\\1", keywords)

    # Use (?<![a-z0-9]) / (?![a-z0-9]) instead of \b so that keywords
    # embedded after underscores (e.g. "io_kjonn") are still matched.
    # Plain \b treats "_" as a word char, which breaks "io_kjonn" vs "kjonn".
    paste0("(?<![a-z0-9])(", paste(unique(keywords), collapse = "|"), ")(?![a-z0-9])")
  })

  keyword_patterns[!vapply(keyword_patterns, is.null, logical(1))]
}
