# =============================================================================
# export_log.R
#
# Audit log export: per-dataset match log, issues log, master log.
#
# Public API (exported):
#   export_match_log()      - orchestrator; writes all log files
#   summarise_master_log()  - read master log and print cross-dataset summary
#
# Internal helpers (not exported, prefixed with `.`):
#   .ensure_log_dir()       - create log directory if it does not exist
#   .add_unmatched_rows()   - append unmatched-column rows to the match log
#   .annotate_log()         - add dataset / year / timestamp / action columns
#   .sort_log()             - sort rows by review priority
#   .reorder_log_cols()     - enforce a canonical column order for readability
#   .write_per_dataset_log()- write the per-file match log CSV
#   .write_issues_log()     - write the issues CSV (coercion / value problems)
#   .update_master_log()    - read existing master, merge, write back
# =============================================================================


# -----------------------------------------------------------------------------
# .ensure_log_dir()
# -----------------------------------------------------------------------------
#' Create the log directory if it does not already exist
#'
#' Extracted so every log-writing helper can call this without duplicating
#' the `dir.exists` / `dir.create` pattern.
#'
#' @param log_dir Path to the log directory.
#'
#' @keywords internal
 
.ensure_log_dir <- function(log_dir) {
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  # Resolve to the absolute path now that the directory exists.
  # normalizePath() requires the path to exist, so this must run after
  # dir.create(). On Windows this expands to e.g. "C:/My Documents/logs".
  # winslash = "/" keeps separators consistent across platforms.
  abs_path <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  message(sprintf("Log directory: %s", abs_path))
  invisible(abs_path)
}


# -----------------------------------------------------------------------------
# .add_unmatched_rows()
# -----------------------------------------------------------------------------
#' 
#' Append unmatched-column rows to the match log
#'
#' Columns that fell through all three matching layers have no entry in
#' `match_result$match_log`.  This helper adds one row per unmatched column
#' so the audit log is a complete record of every column that was in the raw
#' dataset, including those that were dropped.
#'
#' @param log_dt       `data.table` from `match_result$match_log`.
#' @param match_result Full result list from [match_columns()].
#'
#' @return A `data.table` with unmatched rows appended (or the original
#'   `log_dt` if there were no unmatched columns).
#' @keywords internal
.add_unmatched_rows <- function(log_dt, match_result) {
  if (length(match_result$unmatched) == 0) return(log_dt)

  unmatched_dt <- data.table::data.table(
    raw_name     = match_result$unmatched,
    canonical    = NA_character_,
    method       = "unmatched",
    confidence   = "none",
    needs_review = TRUE
  )

  data.table::rbindlist(list(log_dt, unmatched_dt), fill = TRUE, use.names = TRUE)
}


# -----------------------------------------------------------------------------
# .annotate_log()
# -----------------------------------------------------------------------------
#' 
#' Add provenance and action metadata columns to the log
#'
#' Attaches four columns that turn a bare match log into a full audit record:
#' - `dataset`   - which file this row came from
#' - `year`      - survey year for cross-dataset traceability
#' - `logged_at` - ISO-format timestamp of when the log was written
#' - `action`    - human-readable summary of what was done (or should be done)
#'
#' Modifies `log_dt` by reference.
#'
#' @param log_dt       `data.table` to annotate (modified by reference).
#' @param dataset_label String dataset label.
#' @param year_tag     Integer or `NA`.
#' @keywords internal
.annotate_log <- function(log_dt, dataset_label, year_tag) {
  log_dt[, `:=`(
    dataset   = dataset_label,
    year      = year_tag,
    logged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    action    = data.table::fcase(
      method == "exact",         "Renamed to canonical",
      method == "fuzzy",         "Renamed \u2014 verify spelling",
      grepl("^keyword", method), "Predicted \u2014 must verify",
      method == "unmatched",     "DROPPED \u2014 not in canonical set",
      default = "Unknown"
    )
  )]
  invisible(log_dt)
}


# -----------------------------------------------------------------------------
# .sort_log()
# -----------------------------------------------------------------------------
#' 
#' Sort the log so the most important rows appear first
#'
#' Rows flagged for review come before confirmed matches.  Within the
#' review-needed group, lower-confidence matches (none < low < medium) sort
#' above higher-confidence ones so the riskiest decisions are easiest to find
#' when the CSV is opened in a spreadsheet.
#'
#' Modifies `log_dt` by reference.
#'
#' @param log_dt `data.table` to sort (modified by reference).
#' @keywords internal
.sort_log <- function(log_dt) {
  conf_order <- c("none", "low", "medium", "high")
  log_dt[, .conf_rank := match(confidence, conf_order)]
  data.table::setorder(log_dt, -needs_review, .conf_rank)
  log_dt[, .conf_rank := NULL]
  invisible(log_dt)
}


# -----------------------------------------------------------------------------
# .reorder_log_cols()
# -----------------------------------------------------------------------------
#' 
#' Enforce a canonical column order for readability
#'
#' Puts the most useful columns (what file, what year, what happened) first
#' so the CSV is immediately legible without scrolling right.
#'
#' Modifies `log_dt` by reference.
#'
#' @param log_dt `data.table` to reorder (modified by reference).
#' @keywords internal
.reorder_log_cols <- function(log_dt) {
  data.table::setcolorder(log_dt, c(
    "dataset", "year", "raw_name", "canonical",
    "method", "confidence", "needs_review", "action", "logged_at"
  ))
  invisible(log_dt)
}


# -----------------------------------------------------------------------------
# .write_per_dataset_log()
# -----------------------------------------------------------------------------
#' 
#' Write the per-dataset match log to a CSV file
#'
#' Uses `dataset_label` (sanitised for use in filenames) to produce a unique
#' file per dataset so individual files can be inspected without loading the
#' master log.
#'
#' @param log_dt       The annotated, sorted `data.table` to write.
#' @param log_dir      Target directory (must already exist).
#' @param dataset_label String label used in the filename.
#'
#' @return The output file path, invisibly.
#' @keywords internal
.write_per_dataset_log <- function(log_dt, log_dir, dataset_label) {
  safe_label <- gsub("[^A-Za-z0-9_-]", "_", dataset_label)
  path       <- file.path(log_dir, sprintf("match_log_%s.csv", safe_label))
  data.table::fwrite(log_dt, path)
  message(sprintf("  Match log saved: %s", path))
  invisible(path)
}


# -----------------------------------------------------------------------------
# .write_issues_log()
# -----------------------------------------------------------------------------
#' 
#' Write the coercion / value-validation issues to a CSV file
#'
#' Only called when `issues_dt` is non-`NULL` and has at least one row.
#' Stamps the issues with `dataset` and `year` metadata before writing so the
#' issues file is self-contained when opened independently.
#'
#' @param issues_dt    `data.table` of issues from [clean_dataset()].
#' @param log_dir      Target directory (must already exist).
#' @param dataset_label String label used in metadata and the filename.
#' @param year_tag     Integer or `NA`.
#'
#' @return The output file path, invisibly.
#' @keywords internal
.write_issues_log <- function(issues_dt, log_dir, dataset_label, year_tag) {
  if (is.null(issues_dt) || nrow(issues_dt) == 0) return(invisible(NULL))

  issues_dt <- data.table::copy(issues_dt)
  issues_dt[, `:=`(
    dataset   = dataset_label,
    year      = year_tag,
    logged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )]

  safe_label <- gsub("[^A-Za-z0-9_-]", "_", dataset_label)
  path       <- file.path(log_dir, sprintf("issues_%s.csv", safe_label))
  data.table::fwrite(issues_dt, path)
  message(sprintf("  Issues log saved: %s", path))
  invisible(path)
}


# -----------------------------------------------------------------------------
# .update_master_log()
# -----------------------------------------------------------------------------
#' 
#' Merge the current dataset's log rows into the master log and write it back
#'
#' If a master log already exists, any rows from the same `dataset_label` are
#' removed first (re-run safety), then the new rows are appended.  This means
#' re-processing a file updates rather than duplicates its entries.
#'
#' @param log_dt       The current dataset's annotated log `data.table`.
#' @param log_dir      Directory containing `match_log_MASTER.csv`.
#' @param dataset_label String label used to remove stale rows.
#'
#' @return The combined master `data.table`, invisibly.
#' @keywords internal
.update_master_log <- function(log_dt, log_dir, dataset_label) {
  master_path <- file.path(log_dir, "match_log_MASTER.csv")

  combined_dt <- if (file.exists(master_path)) {
    # quote="" prevents fread treating brackets/quotes in method column as CSV quoting
    master_dt <- data.table::fread(master_path, quote = "")
    # fread auto-detects "logged_at" as POSIXct; coerce back to character
    # so it matches the plain string written by .annotate_log()
    if ("logged_at" %in% names(master_dt))
      master_dt[, logged_at := as.character(logged_at)]
    master_dt <- master_dt[master_dt$dataset != dataset_label, ]
    data.table::rbindlist(list(master_dt, log_dt), fill = TRUE, use.names = TRUE)
  } else {
    log_dt
  }

  data.table::fwrite(combined_dt, master_path)
  message(sprintf(
    "  Master log updated: %s (%d total decisions)",
    master_path, nrow(combined_dt)
  ))
  invisible(combined_dt)
}


# -----------------------------------------------------------------------------
# export_match_log()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Export the column matching log to CSV files
#'
#' Takes the result of [match_columns()] and writes two files: a per-dataset
#' match log and (optionally) an issues log from [clean_dataset()].  A master
#' log that accumulates entries across all datasets is also updated.
#'
#' Each concern is handled by a dedicated internal helper so the function body
#' reads as a plain checklist of steps.
#'
#' @param match_result  List returned by [match_columns()].
#' @param issues_dt     `data.table` of issues from [clean_dataset()], or
#'   `NULL` to skip the issues log.
#' @param dataset_label Human-readable label (used in filenames and metadata).
#' @param year_tag      Integer year for traceability. Default `NA`.
#' @param log_dir       Directory where CSV logs are written. Created if absent.
#' @param append_master If `TRUE` (default), updates the master log CSV.
#'
#' @return The annotated log `data.table` for this dataset, invisibly.
#'
#' @export
export_match_log <- function(match_result,
                             issues_dt     = NULL,
                             dataset_label = "",
                             year_tag      = NA,
                             log_dir       = "logs/matching",
                             append_master = TRUE) {

  # 1. Ensure directory exists and resolve to its absolute path.
  #    Every subsequent helper uses abs_log_dir so messages always show
  #    the full path (e.g. C:/My Documents/logs/match_log_x.csv).
  abs_log_dir <- .ensure_log_dir(log_dir)

  # 2. Start from the match log; add rows for unmatched columns
  log_dt <- .add_unmatched_rows(match_result$match_log, match_result)

  # 3. Attach provenance metadata (dataset, year, timestamp, action)
  .annotate_log(log_dt, dataset_label, year_tag)

  # 4. Sort so review-needed / low-confidence rows appear first
  .sort_log(log_dt)

  # 5. Enforce a consistent column order
  .reorder_log_cols(log_dt)

  # 6. Write the per-dataset match log
  .write_per_dataset_log(log_dt, abs_log_dir, dataset_label)

  # 7. Write the issues log (coercion failures, unexpected values)
  .write_issues_log(issues_dt, abs_log_dir, dataset_label, year_tag)

  # 8. Merge into the master log
  if (append_master)
    .update_master_log(log_dt, abs_log_dir, dataset_label)

  invisible(log_dt)
}


# -----------------------------------------------------------------------------
# summarise_master_log()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Print a cross-dataset summary of the master audit log
#'
#' Reads `match_log_MASTER.csv` from `log_dir` and prints three sections:
#' decisions by match method, columns flagged for review per dataset, and a
#' list of every keyword-predicted match that needs human verification.
#'
#' @param log_dir Directory where `match_log_MASTER.csv` is located.
#'
#' @return The master log `data.table`, invisibly.
#'
#' @importFrom data.table fread
#' @export
summarise_master_log <- function(log_dir = "logs/matching") {

  # Resolve to absolute path so the "not found" message shows the full location
  abs_log_dir <- normalizePath(log_dir, winslash = "/", mustWork = FALSE)
  master_path <- file.path(abs_log_dir, "match_log_MASTER.csv")

  if (!file.exists(master_path)) {
    message("No master log found at: ", master_path)
    return(invisible(NULL))
  }

  master_dt <- data.table::fread(master_path, quote = "")
  cat(sprintf("\n\u2554%s\u2557\n", strrep("\u2550", 54)))
  cat("  MASTER AUDIT SUMMARY \u2014 All Datasets\n")
  cat(sprintf("\u255a%s\u255d\n", strrep("\u2550", 54)))

  cat("\n  Decisions by match method (all datasets):\n")
  method_summary <- master_dt[, .N, by = method][order(-N)]
  for (i in seq_len(nrow(method_summary)))
    cat(sprintf("    %-30s %d\n", method_summary$method[i], method_summary$N[i]))

  cat("\n  Columns flagged for review per dataset:\n")
  review_rows    <- master_dt[master_dt$needs_review == TRUE, ]
  review_summary <- review_rows[, .N, by = .(dataset, year)][order(year)]

  if (nrow(review_summary) == 0) {
    cat("    \u2714 No columns flagged for review.\n")
  } else {
    for (i in seq_len(nrow(review_summary)))
      cat(sprintf("    %-35s (year %s): %d column(s)\n",
                  review_summary$dataset[i], review_summary$year[i],
                  review_summary$N[i]))
    cat(sprintf("\n  \u26a0 Total columns needing review: %d\n", nrow(review_rows)))
  }

  predicted <- master_dt[grepl("^keyword", master_dt$method), ]
  if (nrow(predicted) > 0) {
    cat("\n  \u26a0\u26a0 ALL PREDICTED (keyword) MATCHES \u2014 verify these:\n")
    for (i in seq_len(nrow(predicted)))
      cat(sprintf("    [%s | %s]  %-25s -> %s  (%s)\n",
                  predicted$dataset[i], predicted$year[i],
                  predicted$raw_name[i], predicted$canonical[i],
                  predicted$method[i]))
  }

  cat("\n")
  invisible(master_dt)
}
