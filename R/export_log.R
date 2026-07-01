#' Export a column matching log to CSV
#'
#' Writes the matching decisions from [match_columns()] to a per-dataset CSV
#' file and optionally appends to a master audit log that accumulates entries
#' across all datasets processed in the pipeline.
#'
#' @param match_result List returned by [match_columns()].
#' @param issues_dt `data.table` of coercion/value issues returned by
#'   [clean_dataset()]. Pass `NULL` to skip including issues in the log.
#' @param dataset_label Human-readable label (used in filenames and the log).
#' @param year_tag Optional integer year for traceability.
#' @param log_dir Directory where CSV logs are written. Created if absent.
#' @param append_master If `TRUE` (default), appends to a master log CSV.
#'
#' @return Invisibly returns the log `data.table` for the current dataset.
#'
#' @importFrom data.table data.table rbindlist fread fwrite setorder setcolorder
#' @export
export_match_log <- function(match_result,
                             issues_dt      = NULL,
                             dataset_label  = "",
                             year_tag       = NA,
                             log_dir        = "logs/matching",
                             append_master  = TRUE) {

  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
    message(sprintf("Created log directory: %s", log_dir))
  }

  log_dt <- match_result$match_log

  # Include unmatched columns so the audit is complete
  if (length(match_result$unmatched) > 0) {
    unmatched_dt <- data.table::data.table(
      raw_name     = match_result$unmatched,
      canonical    = NA_character_,
      method       = "unmatched",
      confidence   = "none",
      needs_review = TRUE
    )
    log_dt <- data.table::rbindlist(list(log_dt, unmatched_dt), fill = TRUE, use.names = TRUE)
  }

  # Add metadata columns
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

  # Sort: review-needed first, then by ascending confidence
  confidence_order   <- c("none", "low", "medium", "high")
  log_dt[, conf_rank := match(confidence, confidence_order)]
  data.table::setorder(log_dt, -needs_review, conf_rank)
  log_dt[, conf_rank := NULL]

  data.table::setcolorder(log_dt, c(
    "dataset", "year", "raw_name", "canonical",
    "method", "confidence", "needs_review", "action", "logged_at"
  ))

  # Write per-dataset CSV
  safe_label    <- gsub("[^A-Za-z0-9_-]", "_", dataset_label)
  per_file_path <- file.path(log_dir, sprintf("match_log_%s.csv", safe_label))
  data.table::fwrite(log_dt, per_file_path)
  message(sprintf("  Match log saved: %s", per_file_path))

  # Write issues CSV alongside if provided
  if (!is.null(issues_dt) && nrow(issues_dt) > 0) {
    issues_path <- file.path(log_dir, sprintf("issues_%s.csv", safe_label))
    issues_dt[, `:=`(dataset = dataset_label, year = year_tag,
                      logged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))]
    data.table::fwrite(issues_dt, issues_path)
    message(sprintf("  Issues log saved: %s", issues_path))
  }

  # Append to master log
  if (append_master) {
    master_path <- file.path(log_dir, "match_log_MASTER.csv")
    combined_dt <- if (file.exists(master_path)) {
      master_dt <- data.table::fread(master_path)
      master_dt <- master_dt[master_dt$dataset != dataset_label, ]
      data.table::rbindlist(list(master_dt, log_dt), fill = TRUE, use.names = TRUE)
    } else {
      log_dt
    }
    data.table::fwrite(combined_dt, master_path)
    message(sprintf("  Master log updated: %s (%d total decisions)", master_path, nrow(combined_dt)))
  }

  invisible(log_dt)
}


#' Print a summary of the master audit log
#'
#' Reads the master log written by [export_match_log()] and prints a
#' high-level summary of all matching decisions across datasets, highlighting
#' columns that need manual review.
#'
#' @param log_dir Directory where the master log CSV is located.
#'
#' @return Invisibly returns the master log `data.table`.
#'
#' @importFrom data.table fread
#' @export
summarise_master_log <- function(log_dir = "logs/matching") {

  master_path <- file.path(log_dir, "match_log_MASTER.csv")

  if (!file.exists(master_path)) {
    message("No master log found at: ", master_path)
    return(invisible(NULL))
  }

  master_dt <- data.table::fread(master_path)

  
  
  cat(sprintf("\n\u2554%s\u2557\n", strrep("\u2550", 47)))
  cat("  MASTER AUDIT SUMMARY \u2014 All Datasets\n")
  cat(sprintf("\u255a%s\u255d\n", strrep("\u2550", 47)))

  cat("\n  Decisions by match method (all datasets):\n")
  method_summary <- master_dt[, .N, by = method][order(-N)]
  for (i in seq_len(nrow(method_summary))) {
    cat(sprintf("    %-30s %d\n", method_summary$method[i], method_summary$N[i]))
  }

  cat("\n  Columns flagged for review per dataset:\n")
  review_rows    <- master_dt[master_dt$needs_review == TRUE, ]
  review_summary <- review_rows[, .N, by = .(dataset, year)][order(year)]

  if (nrow(review_summary) == 0) {
    cat("    \u2714 No columns flagged for review.\n")
  } else {
    for (i in seq_len(nrow(review_summary))) {
      cat(sprintf("    %-35s (year %s): %d column(s)\n",
                  review_summary$dataset[i], review_summary$year[i], review_summary$N[i]))
    }
    cat(sprintf("\n  \u26a0 Total columns needing review: %d\n", nrow(review_rows)))
  }

  predicted <- master_dt[grepl("^keyword", master_dt$method), ]
  if (nrow(predicted) > 0) {
    cat("\n  \u26a0\u26a0 ALL PREDICTED (keyword) MATCHES \u2014 verify these:\n")
    for (i in seq_len(nrow(predicted))) {
      cat(sprintf("    [%s | %s]  %-25s -> %s  (%s)\n",
                  predicted$dataset[i], predicted$year[i],
                  predicted$raw_name[i], predicted$canonical[i], predicted$method[i]))
    }
  }

  cat("\n")
  invisible(master_dt)
}
