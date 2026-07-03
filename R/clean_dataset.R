# =============================================================================
# clean_dataset.R
#
# Survey dataset cleaning pipeline.
#
# Public API:
#   clean_dataset()          - orchestrator; applies all steps in order
#
# Internal helpers:
#   .normalise_colnames()    - step 1: lowercase + snake_case column names
#   .apply_renames()         - step 3: rename matched columns in-place
#   .select_canonical_cols() - step 4: drop non-canonical columns
#   .apply_recodes()         - step 5: remap raw values via recode_map
#   .apply_missing_codes()   - step 6: replace declared missing codes with NA
#   .coerce_column()         - step 7 (single col): safe type conversion
#   .coerce_all_columns()    - step 7 (all cols): iterate + collect issues
#   .validate_column_values()- step 8 (single col): check against value_map
#   .validate_all_values()   - step 8 (all cols): iterate + collect issues
#   .inject_year()           - step 9: add integer year column
#   .drop_empty_rows()       - step 10: remove fully-NA and duplicate rows
#   .compile_issues()        - assemble final issues data.table
#   .clean_names_dt()        - internal clean_names implementation
# =============================================================================


# -----------------------------------------------------------------------------
# .normalise_colnames()
# -----------------------------------------------------------------------------
#' 
#' Step 1 - normalise column names to snake_case
#'
#' Converts all column names to lowercase, replaces any run of non-alphanumeric
#' characters with a single underscore, and strips leading/trailing underscores.
#' Duplicate names are disambiguated with a numeric suffix (same behaviour as
#' `janitor::clean_names()`).
#'
#' Kept as a standalone helper so it can be tested in isolation and reused
#' anywhere a data.table needs name normalisation without loading janitor.
#'
#' @param dt A data.table.
#' @return The same data.table with normalised column names (modified by
#'   reference via [data.table::setnames()]).
#' @keywords internal
.normalise_colnames <- function(dt) {
  old <- names(dt)
  new <- tolower(old)
  new <- gsub("[^a-z0-9]+", "_", new)
  new <- gsub("^_+|_+$", "", new)
  new <- make.unique(new, sep = "_")
  data.table::setnames(dt, old, new)
  dt
}

# Alias used historically inside the package
.clean_names_dt <- function(df) {
  dt <- data.table::as.data.table(df)
  .normalise_colnames(dt)
}


# -----------------------------------------------------------------------------
# .apply_renames()
# -----------------------------------------------------------------------------
#' 
#' Step 3 - rename matched columns to their canonical names
#'
#' Applies `rename_vec` (a named character vector `old -> new`) to `dt` using
#' [data.table::setnames()].  Only renames entries where the old name actually
#' exists in `dt`; silently skips the rest so this function is safe to call
#' with a rename_vec built from a different dataset.
#'
#' @param dt         A data.table (modified by reference).
#' @param rename_vec Named character vector from `match_result$rename_vec`.
#' @return `dt` invisibly (modification is in-place).
.apply_renames <- function(dt, rename_vec) {
  if (length(rename_vec) == 0) return(invisible(dt))

  valid <- rename_vec[names(rename_vec) %in% names(dt)]
  if (length(valid) > 0)
    data.table::setnames(dt, names(valid), unname(valid))

  invisible(dt)
}


# -----------------------------------------------------------------------------
# .select_canonical_cols()
# -----------------------------------------------------------------------------
#' 
#' Step 4 - keep only columns whose name is a canonical variable name
#'
#' Any column not present in `names(config$var_map)` is dropped.  This is
#' the step that removes unmatched columns from the output - if a column was
#' never renamed to its canonical name (because matching failed), it will not
#' appear in this intersection and will be silently excluded.
#'
#' @param dt     A data.table.
#' @param config Config object from [load_config()].
#' @return A new data.table containing only canonical columns.
#' @keywords internal
.select_canonical_cols <- function(dt, config) {
  keep <- intersect(names(dt), names(config$var_map))
  dt[, keep, with = FALSE]
}


# -----------------------------------------------------------------------------
# .apply_recodes()
# -----------------------------------------------------------------------------
#' 
#' Step 5 - recode raw values using the recode_map
#'
#' For each variable with a `recode:` block in the YAML, builds a lookup
#' vector and replaces matching cell values.  Recoding runs on the string
#' representation of the column so it is independent of the column's current
#' type - this matters because type coercion happens in step 7.
#'
#' @param dt     A data.table (modified by reference).
#' @param config Config object from [load_config()].
#' @param cols   Character vector of column names to process.
#' @return `dt` invisibly.
#' @keywords internal
.apply_recodes <- function(dt, config, cols) {
  for (var in cols) {
    rc <- config$recode_map[[var]]
    if (is.null(rc)) next
    
    raw_char  <- as.character(dt[[var]])
    recode_lk <- stats::setNames(rc$new_value, rc$raw_value)
    recoded   <- recode_lk[raw_char]
    changed   <- !is.na(recoded)
    if (any(changed)) dt[changed, (var) := recoded[changed]]
  }
  invisible(dt)
}


# -----------------------------------------------------------------------------
# .apply_missing_codes()
# -----------------------------------------------------------------------------
#' 
#' Step 6 - replace declared numeric missing codes with NA
#'
#' Only replaces integer codes listed in the `missing:` YAML block.  Literal
#' `NA` entries in the YAML (written as `~`) are ignored here because those
#' cells are already `NA` in R.
#'
#' This step is intentionally opt-in (`apply_missing = TRUE` in
#' [clean_dataset()]) because silently converting plausible integers like 8
#' or 9 to `NA` can mask data problems.
#'
#' @param dt     A data.table (modified by reference).
#' @param config Config object from [load_config()].
#' @param cols   Character vector of column names to process.
#' @return `dt` invisibly.
#' @keywords internal
.apply_missing_codes <- function(dt, config, cols) {
  for (var in cols) {
    codes <- config$missing_map[[var]]
    if (is.null(codes) || length(codes) == 0) next

    int_codes <- suppressWarnings(as.integer(codes))
    int_codes <- int_codes[!is.na(int_codes)]
    if (length(int_codes) > 0)
      dt[dt[[var]] %in% int_codes, (var) := NA]
  }
  invisible(dt)
}


# -----------------------------------------------------------------------------
# .coerce_column()
# -----------------------------------------------------------------------------
#'
#' Step 7 (single column) - attempt safe type coercion
#'
#' Converts `x` to `target_type` and checks whether the conversion introduced
#' any *new* `NA` values (i.e. values that were non-missing before but became
#' `NA` after casting).  Returns a list so the caller can decide whether to
#' apply the result or record an issue.
#'
#' Separating the attempt from the application means [.coerce_all_columns()]
#' never modifies the table for a column that would silently lose data.
#'
#' @param x           A vector (one column from the data.table).
#' @param target_type String: "integer", "numeric", "double", "character",
#'   or "logical".
#' @param var         Variable name (used in issue messages only).
#' @param dataset_label Dataset label for warning messages.
#'
#' @return A list:
#' \describe{
#'   \item{converted}{The coerced vector, or `NULL` for unknown types.}
#'   \item{success}{`TRUE` if coercion is safe (no new NAs).}
#'   \item{issue}{A one-row data.table describing the problem, or `NULL`.}
#' }
#' @keywords internal
.coerce_column <- function(x, target_type, var, dataset_label = "") {

  converted <- switch(
    target_type,
    "integer"   = suppressWarnings(as.integer(x)),
    "numeric"   = suppressWarnings(as.numeric(x)),
    "double"    = suppressWarnings(as.double(x)),
    "character" = as.character(x),
    "logical"   = suppressWarnings(as.logical(x)),
    NULL
  )

  # Unknown type - caller should log a skip
  if (is.null(converted)) {
    return(list(
      converted = NULL,
      success   = FALSE,
      issue     = data.table::data.table(
        variable   = var,
        issue_type = "coercion_skip",
        detail     = sprintf("Unknown target type '%s'; column left as-is.", target_type)
      )
    ))
  }

  new_na <- sum(is.na(converted) & !is.na(x))

  if (new_na == 0) {
    return(list(converted = converted, success = TRUE, issue = NULL))
  }

  # Coercion would introduce NAs - collect details for the issue log
  bad_vals <- unique(as.character(x[is.na(converted) & !is.na(x)]))
  if (length(bad_vals) > 5) bad_vals <- c(bad_vals[1:5], "...")

  warning(sprintf(
    "[%s] '%s': cannot coerce %d value(s) to %s without NAs. Bad values: %s. Kept as %s.",
    dataset_label, var, new_na, target_type,
    paste(bad_vals, collapse = ", "), class(x)[1]
  ), call. = FALSE)

  list(
    converted = NULL,
    success   = FALSE,
    issue     = data.table::data.table(
      variable   = var,
      issue_type = "coercion_failure",
      detail     = sprintf(
        "Target: %s. %d value(s) -> NA: %s. Column kept as %s.",
        target_type, new_na, paste(bad_vals, collapse = ", "), class(x)[1]
      )
    )
  )
}


# -----------------------------------------------------------------------------
# .coerce_all_columns()
# -----------------------------------------------------------------------------
#' 
#' Step 7 (all columns) - apply safe coercion across the dataset
#'
#' Iterates over `cols`, calls [.coerce_column()] for each, applies the result
#' only when coercion is safe, and accumulates issue rows for every column
#' where it is not.
#'
#' @param dt           A data.table (modified by reference for safe columns).
#' @param config       Config object from [load_config()].
#' @param cols         Character vector of column names to process.
#' @param dataset_label Label for warning messages.
#'
#' @return A list of issue data.tables (may be empty).
#' @keywords internal
.coerce_all_columns <- function(dt, config, cols, dataset_label = "") {
  issues <- list()

  for (var in cols) {
    target <- config$type_map[[var]]
    if (is.null(target)) next

    result <- .coerce_column(dt[[var]], target, var, dataset_label)

    if (result$success) {
      dt[, (var) := result$converted]
    } else if (!is.null(result$issue)) {
      issues[[length(issues) + 1]] <- result$issue
    }
  }

  issues
}


# -----------------------------------------------------------------------------
# .validate_column_values()
# -----------------------------------------------------------------------------
#' 
#' Step 8 (single column) - check observed values against the declared set
#'
#' When the YAML declares a `value:` block, every non-missing cell must be one
#' of the listed integer codes.  Values outside that set are unexpected and
#' may indicate a recoding error, a new survey version, or missing-code
#' contamination.
#'
#' Comparison is done on the integer representation of both sides so that a
#' character column `"1"` is correctly matched against numeric key `1`.
#'
#' @param col_vec   Vector of values from one column.
#' @param valid_set Integer vector of valid values from `config$value_map`.
#' @param var       Variable name.
#' @param dataset_label Label for warning messages.
#'
#' @return A one-row issue data.table if unexpected values exist, otherwise
#'   `NULL`.
#' @keywords internal
.validate_column_values <- function(col_vec, valid_set, var, dataset_label = "") {
  non_missing  <- col_vec[!is.na(col_vec)]
  as_int       <- suppressWarnings(as.integer(non_missing))
  unexpected   <- setdiff(as_int[!is.na(as_int)], valid_set)

  if (length(unexpected) == 0) return(NULL)

  warning(sprintf(
    "[%s] '%s': %d unexpected value(s) not in YAML 'value:' list: %s.",
    dataset_label, var, length(unexpected), paste(unexpected, collapse = ", ")
  ), call. = FALSE)

  data.table::data.table(
    variable   = var,
    issue_type = "unexpected_values",
    detail     = sprintf(
      "Values not declared in YAML: %s. Valid set: %s.",
      paste(unexpected, collapse = ", "),
      paste(valid_set,  collapse = ", ")
    )
  )
}


# -----------------------------------------------------------------------------
# .validate_all_values()
# -----------------------------------------------------------------------------
#' 
#' Step 8 (all columns) - value conformance check across the dataset
#'
#' Iterates over `cols`, skips variables with no `value:` block, and calls
#' [.validate_column_values()] for the rest.
#'
#' @param dt           A data.table (read-only in this step).
#' @param config       Config object from [load_config()].
#' @param cols         Character vector of column names to check.
#' @param dataset_label Label for warning messages.
#'
#' @return A list of issue data.tables (may be empty).
#' @keywords internal
.validate_all_values <- function(dt, config, cols, dataset_label = "") {
  issues <- list()

  for (var in cols) {
    vt <- config$value_map[[var]]
    if (is.null(vt)) next

    issue <- .validate_column_values(dt[[var]], vt$num_value, var, dataset_label)
    if (!is.null(issue)) issues[[length(issues) + 1]] <- issue
  }

  issues
}


# -----------------------------------------------------------------------------
# .inject_year()
# -----------------------------------------------------------------------------
#' 
#' Step 9 - add an integer year column
#'
#' Adds `year` as the first column of `dt` by reference.  When stacking
#' multiple annual datasets the year column is the primary grouping variable.
#' No-op when `year_tag` is `NULL`.
#'
#' @param dt       A data.table (modified by reference).
#' @param year_tag Integer or `NULL`.
#' @return `dt` invisibly.
#' @keywords internal
.inject_year <- function(dt, year_tag) {
  if (!is.null(year_tag))
    dt[, year := as.integer(year_tag)]
  invisible(dt)
}


# -----------------------------------------------------------------------------
# .drop_empty_rows()
# -----------------------------------------------------------------------------
#' 
#' Step 10 - remove duplicate and fully-empty rows
#'
#' First deduplicates with [data.table::unique()], then drops rows where every
#' column is `NA` (these contribute no information and inflate row counts).
#'
#' @param dt A data.table.
#' @return A new data.table with empty/duplicate rows removed.
#' @keywords internal
.drop_empty_rows <- function(dt) {
  dt <- unique(dt)
  dt[rowSums(!is.na(dt)) > 0]
}


# -----------------------------------------------------------------------------
# .compile_issues()
# -----------------------------------------------------------------------------
#' 
#' Assemble a flat issues data.table from a list of one-row data.tables
#'
#' Called at the end of [clean_dataset()] after both coercion and value
#' validation have run.  Returns an empty data.table with the correct schema
#' when there are no issues, so callers never need to `NULL`-check the result.
#'
#' @return A data.table with columns `variable`, `issue_type`, `detail`.
#' @keywords internal
.compile_issues <- function(...) {
  all_issues <- unlist(list(...), recursive = FALSE)

  if (length(all_issues) == 0) {
    return(data.table::data.table(
      variable   = character(),
      issue_type = character(),
      detail     = character()
    ))
  }

  data.table::rbindlist(all_issues, fill = TRUE, use.names = TRUE)
}


# -----------------------------------------------------------------------------
# clean_dataset()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Clean and standardize a raw survey dataset
#'
#' Orchestrates the full cleaning pipeline for a single dataset.  Each step
#' is delegated to a focused internal helper (see `clean_dataset.R` source)
#' so individual steps can be tested and extended independently.
#'
#' Steps in order:
#' \enumerate{
#'   \item Normalise column names ([.normalise_colnames()])
#'   \item Match columns to canonical names ([match_columns()])
#'   \item Apply renames ([.apply_renames()])
#'   \item Keep only canonical columns ([.select_canonical_cols()])
#'   \item Recode values ([.apply_recodes()])
#'   \item Optionally replace missing codes with NA ([.apply_missing_codes()])
#'   \item Safe type coercion ([.coerce_all_columns()])
#'   \item Value conformance validation ([.validate_all_values()])
#'   \item Inject year column ([.inject_year()])
#'   \item Remove duplicate/empty rows ([.drop_empty_rows()])
#' }
#'
#' @param dt            A `data.table` (or coercible object).
#' @param config        Config object returned by [load_config()].
#' @param year_tag      Optional integer year added as a `year` column.
#' @param dataset_label Human-readable label used in warnings and logs.
#' @param apply_missing If `TRUE`, recode numeric missing codes to `NA`.
#'   Default `FALSE` (explicit opt-in to avoid silent data loss).
#'
#' @return A list:
#' \describe{
#'   \item{data}{The cleaned `data.table`.}
#'   \item{issues}{`data.table` with columns `variable`, `issue_type`,
#'     `detail` - one row per coercion failure or value violation.}
#' }
#'
#' @examples
#' \dontrun{
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#' csv <- system.file("extdata", "test-data.csv",    package = "cuci")
#' cfg <- load_config(yml)
#' raw <- data.table::fread(csv)
#' out <- clean_dataset(raw, cfg, year_tag = 2023, dataset_label = "test")
#' out$data
#' out$issues
#' }
#'
#' @importFrom data.table copy as.data.table setnames data.table rbindlist
#' @export
clean_dataset <- function(dt,
                          config,
                          year_tag      = NULL,
                          dataset_label = "",
                          apply_missing = FALSE) {

  dt <- data.table::copy(data.table::as.data.table(dt))

  # 1. Normalise column names
  .normalise_colnames(dt)

  # 2. Match columns (3-layer)
  match_result <- match_columns(names(dt), config)
  print_match_report(match_result, dataset_label)

  # 3. Apply renames
  .apply_renames(dt, match_result$rename_vec)

  # 4. Keep only canonical columns
  dt <- .select_canonical_cols(dt, config)
  cols <- names(dt)

  # 5. Recode values
  .apply_recodes(dt, config, cols)

  # 6. Optional: replace missing codes with NA
  if (apply_missing) .apply_missing_codes(dt, config, cols)

  # 7. Safe type coercion - issues collected, not thrown
  coerce_issues <- .coerce_all_columns(dt, config, cols, dataset_label)

  # 8. Value conformance validation
  value_issues <- .validate_all_values(dt, config, cols, dataset_label)

  # 9. Inject year
  .inject_year(dt, year_tag)

  # 10. Drop duplicates and fully-empty rows
  dt <- .drop_empty_rows(dt)

  list(
    data   = dt,
    issues = .compile_issues(coerce_issues, value_issues)
  )
}
