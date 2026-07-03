# =============================================================================
# validate_dataset.R
#
# Post-clean validation: structural checks and value conformance.
#
# Public API (exported):
#   validate_dataset()          - orchestrator; prints full report, returns dt
#
# Internal helpers (not exported, prefixed with `.`):
#   .validate_print_header()    - print the report header box
#   .validate_structure()       - row count, column list, missingness per col
#   .validate_one_value_col()   - conformance check for a single column
#   .validate_all_value_cols()  - iterate over all columns with a value_map
#   .validate_print_footer()    - closing separator line
# =============================================================================


# -----------------------------------------------------------------------------
# .validate_print_header()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' 
#' Print the validation report header
#'
#' Prints a fixed-width separator and the dataset label.  Extracted so the
#' separator character count lives in exactly one place.
#'
#' @param dataset_label String label for the dataset.
#' @param width Integer width of the separator line. Default `48`.
.validate_print_header <- function(dataset_label, width = 48L) {
  cat(sprintf("\n%s\n", strrep("=", width)))
  cat(sprintf(" Validation Report: %s\n", dataset_label))
  cat(sprintf("%s\n", strrep("=", width)))
}


# -----------------------------------------------------------------------------
# .validate_structure()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Report structural properties of the cleaned dataset
#'
#' Prints row count, column names, and missingness percentage per column.
#' Columns with more than 50% missing values are flagged with an arrow so
#' they stand out when scanning a long report.
#'
#' @param dt A data.table.
.validate_structure <- function(dt) {
  cat(sprintf("  Rows    : %d\n", nrow(dt)))
  cat(sprintf("  Columns : %s\n\n", paste(names(dt), collapse = ", ")))

  cat("  Missingness per column: (<-- HIGH indicates >50% missing)\n")
  for (col in names(dt)) {
    pct  <- round(mean(is.na(dt[[col]])) * 100, 1)
    flag <- if (pct > 50) "  <--" else ""
    cat(sprintf("    %-20s %s%%%s\n", col, pct, flag))
  }
}


# -----------------------------------------------------------------------------
# .validate_one_value_col()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' 
#' Check one column's observed values against the declared valid set
#'
#' Compares the non-missing integer values in `col_vec` against `valid_set`
#' and returns any values that appear in the data but were not declared in the
#' YAML `value:` block.
#'
#' Returning the unexpected values (rather than printing directly) keeps this
#' function pure - the caller decides how to present the result.
#'
#' @param col_vec   Vector of values from one column.
#' @param valid_set Integer vector of declared valid values.
#'
#' @return Integer vector of unexpected values, length 0 if all values are
#'   valid.
.validate_one_value_col <- function(col_vec, valid_set) {
  non_missing <- col_vec[!is.na(col_vec)]
  as_int      <- suppressWarnings(as.integer(non_missing))
  setdiff(as_int[!is.na(as_int)], valid_set)
}


# -----------------------------------------------------------------------------
# .validate_all_value_cols()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' 
#' Check all columns that have a declared value set
#'
#' Iterates over the intersection of `dt`'s columns and `config$value_map`,
#' calls [.validate_one_value_col()] for each, and prints a report section.
#' Variables without a `value:` block are silently skipped.
#'
#' @param dt     A data.table (read-only).
#' @param config Config object from [load_config()].
.validate_all_value_cols <- function(dt, config) {
  any_issue <- FALSE

  for (var in intersect(names(dt), names(config$value_map))) {
    value_tbl <- config$value_map[[var]]
    if (is.null(value_tbl)) next

    unexpected <- .validate_one_value_col(dt[[var]], value_tbl$num_value)

    if (length(unexpected) > 0) {
      if (!any_issue) {
        cat("\n  Value conformance issues:\n")
        any_issue <- TRUE
      }
      cat(sprintf(
        "    \u26a0 %-20s unexpected: %-15s valid: %s\n",
        var,
        paste(unexpected,         collapse = ", "),
        paste(value_tbl$num_value, collapse = ", ")
      ))
    }
  }

  if (!any_issue)
    cat("\n  \u2714 All value conformance checks passed.\n")
}


# -----------------------------------------------------------------------------
# .validate_print_footer()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Print the closing separator for the validation report
#'
#' @param width Integer width of the separator line. Default `48`.
.validate_print_footer <- function(width = 48L) {
  cat(sprintf("%s\n", strrep("=", width)))
}


# -----------------------------------------------------------------------------
# validate_dataset()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Validate a cleaned survey dataset against the variable map
#'
#' Runs structural checks (row count, column list, missingness) and - when a
#' config is supplied - value-conformance checks against the `value:` blocks
#' in the YAML.  Each concern is handled by a dedicated internal helper so
#' individual checks can be tested and extended without touching the others.
#'
#' @param dt            A cleaned `data.table` (output of [clean_dataset()]).
#' @param config        Config object from [load_config()]. Pass `NULL` to
#'   skip value conformance checks.
#' @param dataset_label Optional string label for the report heading.
#'
#' @return `dt` invisibly, so the function can be used inline in a pipeline.
#'
#' @examples
#' \dontrun{
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#' csv <- system.file("extdata", "test-data.csv",    package = "cuci")
#' cfg <- load_config(yml)
#' raw <- data.table::fread(csv)
#' out <- clean_dataset(raw, cfg)
#' validate_dataset(out$data, cfg, "test-data")
#' }
#'
#' @importFrom data.table as.data.table
#' @export
validate_dataset <- function(dt, config = NULL, dataset_label = "") {

  .validate_print_header(dataset_label)
  .validate_structure(dt)

  if (!is.null(config))
    .validate_all_value_cols(dt, config)

  .validate_print_footer()

  invisible(dt)
}
