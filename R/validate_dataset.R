#' Validate a cleaned survey dataset against the variable map
#'
#' Runs sanity checks on a cleaned dataset and prints a structured report.
#' Checks include: row/column counts, missingness per column, and — when the
#' variable map declares valid values — conformance of every non-missing cell.
#'
#' @param dt A cleaned `data.table` (output of [clean_dataset()]).
#' @param config Config object returned by [load_config()]. If `NULL`, only
#'   structural checks (row count, missingness) are performed.
#' @param dataset_label Optional string label used in the report heading.
#'
#' @return The input `dt` invisibly, so the function can be used inline in a
#'   pipeline.
#'
#' @examples
#' \dontrun{
#' yml  <- system.file("extdata", "variable_map.yml", package = "cuci")
#' csv  <- system.file("extdata", "test-data.csv",    package = "cuci")
#' cfg  <- load_config(yml)
#' raw  <- data.table::fread(csv)
#' out  <- clean_dataset(raw, cfg)
#' validate_dataset(out$data, cfg, "test-data")
#' }
#'
#' @importFrom data.table as.data.table
#' @export
validate_dataset <- function(dt, config = NULL, dataset_label = "") {

  cat(sprintf("\n%s\n", strrep("=", 48)))
  cat(sprintf(" Validation Report: %s\n", dataset_label))
  cat(sprintf("%s\n", strrep("=", 48)))
  cat(sprintf("  Rows    : %d\n", nrow(dt)))
  cat(sprintf("  Columns : %s\n\n", paste(names(dt), collapse = ", ")))

  # --- Missingness per column -------------------------------------------
  cat("  Missingness per column: (arrow <-- indicates HIGH missing)\n")
  for (col in names(dt)) {
    pct <- round(mean(is.na(dt[[col]])) * 100, 1)
    flag <- if (pct > 50) "  <-- " else ""
    cat(sprintf("    %-20s %s%%%s\n", col, pct, flag))
  }

  # --- Value conformance checks (requires config) -----------------------
  if (!is.null(config)) {
    any_value_issue <- FALSE

    for (var in intersect(names(dt), names(config$value_map))) {
      value_tbl <- config$value_map[[var]]
      if (is.null(value_tbl)) next

      valid_vals  <- value_tbl$num_value
      col_vals    <- suppressWarnings(as.integer(dt[[var]]))
      non_missing <- col_vals[!is.na(col_vals)]
      unexpected  <- setdiff(non_missing, valid_vals)

      if (length(unexpected) > 0) {
        if (!any_value_issue) {
          cat("\n  Value conformance issues:\n")
          any_value_issue <- TRUE
        }
        cat(sprintf(
          "    \u26a0 %-20s unexpected: %-15s valid: %s\n",
          var,
          paste(unexpected, collapse = ", "),
          paste(valid_vals, collapse = ", ")
        ))
      }
    }

    if (!any_value_issue) {
      cat("\n  \u2714 All value conformance checks passed.\n")
    }
  }

  cat(sprintf("%s\n", strrep("=", 48)))

  invisible(dt)
}
