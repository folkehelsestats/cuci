#' Clean and standardize a raw survey dataset
#'
#' Applies the full standardization pipeline to a single raw dataset:
#' column-name normalization, intelligent matching, renaming, recoding,
#' safe type coercion, value validation, and optional missing-value recoding.
#'
#' @section Type coercion:
#' Coercion is *safe*: before converting a column the function checks whether
#' any values would silently become `NA` (the classic "NAs introduced by
#' coercion" warning). Columns that cannot be fully coerced are left at their
#' original type and recorded in the returned `coercion_issues` table.
#'
#' @section Value validation:
#' When a variable has a `value:` block in the YAML, every non-missing cell
#' is checked against the declared set of valid values. Unexpected values
#' trigger a warning and are recorded in `value_issues`.
#'
#' @param dt A `data.table` (or coercible object) containing the raw data.
#' @param config Config object returned by [load_config()].
#' @param year_tag Optional integer year to inject as a `year` column.
#' @param dataset_label Human-readable label used in warnings and logs.
#' @param apply_missing Logical. If `TRUE`, recode declared missing values
#'   (numeric codes only) to `NA`. Default `FALSE` — explicit opt-in.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{data}{The cleaned `data.table`.}
#'   \item{issues}{A `data.table` summarising coercion failures and value
#'     violations, with columns `variable`, `issue_type`, and `detail`.}
#' }
#'
#' @examples
#' \dontrun{
#' yml  <- system.file("extdata", "variable_map.yml", package = "cuci")
#' csv  <- system.file("extdata", "test-data.csv",    package = "cuci")
#' cfg  <- load_config(yml)
#' raw  <- data.table::fread(csv)
#' out  <- clean_dataset(raw, cfg, year_tag = 2023, dataset_label = "test")
#' out$data
#' out$issues
#' }
#'
#' @importFrom data.table copy as.data.table setnames data.table rbindlist
#' @importFrom janitor clean_names
#' @export
clean_dataset <- function(dt,
                          config,
                          year_tag      = NULL,
                          dataset_label = "",
                          apply_missing = FALSE) {

  dt <- data.table::copy(data.table::as.data.table(dt))

  # Accumulate issues across all variables
  issue_log <- list()

  # ---- Step 1: Normalize column names ----------------------------------
  dt <- data.table::as.data.table(.clean_names_dt(as.data.frame(dt)))

  # ---- Step 2: Intelligent column matching (3-layer) -------------------
  match_result <- match_columns(names(dt), config)
  print_match_report(match_result, dataset_label)

  # ---- Step 3: Apply renames -------------------------------------------
  if (length(match_result$rename_vec) > 0) {
    existing_renames <- match_result$rename_vec[
      names(match_result$rename_vec) %in% names(dt)
    ]
    if (length(existing_renames) > 0) {
      data.table::setnames(dt,
        old = names(existing_renames),
        new = unname(existing_renames)
      )
    }
  }

  # ---- Step 4: Keep only canonical variables ---------------------------
  keep_cols <- intersect(names(dt), names(config$var_map))
  dt        <- dt[, keep_cols, with = FALSE]

  # ---- Step 5: Recode values -------------------------------------------
  for (var in keep_cols) {
    recode_tbl <- config$recode_map[[var]]
    if (!is.null(recode_tbl)) {
      raw_as_char <- as.character(dt[[var]])
      recode_vec  <- setNames(recode_tbl$new_value, recode_tbl$raw_value)
      recoded     <- recode_vec[raw_as_char]
      matched     <- !is.na(recoded)
      if (any(matched)) dt[matched, (var) := recoded[matched]]
    }
  }

  # ---- Step 6: Optional missing-value recoding -------------------------
  # Only numeric codes are recoded; literal NA is already NA.
  if (apply_missing) {
    for (var in keep_cols) {
      miss_codes <- config$missing_map[[var]]
      if (!is.null(miss_codes) && length(miss_codes) > 0) {
        miss_int <- suppressWarnings(as.integer(miss_codes))
        miss_int <- miss_int[!is.na(miss_int)]
        if (length(miss_int) > 0) {
          dt[dt[[var]] %in% miss_int, (var) := NA]
        }
      }
    }
  }

  # ---- Step 7: Safe type coercion --------------------------------------
  for (var in keep_cols) {
    target_type <- config$type_map[[var]]
    original    <- dt[[var]]

    converted <- .safe_coerce(original, target_type)

    if (is.null(converted)) {
      # Unsupported type spec — skip
      issue_log[[length(issue_log) + 1]] <- data.table::data.table(
        variable   = var,
        issue_type = "coercion_skip",
        detail     = sprintf("Unknown target type '%s'; column left as-is.", target_type)
      )
      next
    }

    # Check whether coercion introduced NEW NAs (beyond pre-existing ones)
    pre_na  <- is.na(original)
    post_na <- is.na(converted)
    new_na  <- sum(post_na & !pre_na)

    if (new_na > 0) {
      # Don't apply — keep original type and report
      bad_vals <- unique(as.character(original[post_na & !pre_na]))
      if (length(bad_vals) > 5) bad_vals <- c(bad_vals[1:5], "...")

      warning(sprintf(
        "[%s] Column '%s': cannot coerce %d value(s) to %s without introducing NAs. ",
        dataset_label, var, new_na, target_type
      ), sprintf("Problematic values: %s. Column kept as %s.",
        paste(bad_vals, collapse = ", "), class(original)[1]),
        call. = FALSE
      )

      issue_log[[length(issue_log) + 1]] <- data.table::data.table(
        variable   = var,
        issue_type = "coercion_failure",
        detail     = sprintf(
          "Target type: %s. %d value(s) would become NA: %s. Column kept as %s.",
          target_type, new_na,
          paste(unique(as.character(original[post_na & !pre_na]))[1:min(5, new_na)],
                collapse = ", "),
          class(original)[1]
        )
      )
    } else {
      # Safe — apply coercion
      dt[, (var) := converted]
    }
  }

  # ---- Step 8: Value validation ----------------------------------------
  for (var in keep_cols) {
    value_tbl <- config$value_map[[var]]
    if (is.null(value_tbl)) next

    valid_vals  <- value_tbl$num_value
    col_vals    <- dt[[var]]

    # Only check non-missing values
    non_missing <- col_vals[!is.na(col_vals)]
    # Compare as integer for consistency
    unexpected  <- setdiff(suppressWarnings(as.integer(non_missing)), valid_vals)

    if (length(unexpected) > 0) {
      warning(sprintf(
        "[%s] Column '%s': found %d unexpected value(s) not in YAML 'value:' list: %s.",
        dataset_label, var, length(unexpected),
        paste(unexpected, collapse = ", ")
      ), call. = FALSE)

      issue_log[[length(issue_log) + 1]] <- data.table::data.table(
        variable   = var,
        issue_type = "unexpected_values",
        detail     = sprintf(
          "Values not declared in YAML: %s. Valid set: %s.",
          paste(unexpected, collapse = ", "),
          paste(valid_vals, collapse = ", ")
        )
      )
    }
  }

  # ---- Step 9: Inject year if provided ---------------------------------
  if (!is.null(year_tag)) {
    dt[, year := as.integer(year_tag)]
  }

  # ---- Step 10: Remove duplicates and fully empty rows -----------------
  dt <- unique(dt)
  dt <- dt[rowSums(is.na(dt)) < ncol(dt)]

  # ---- Compile issues table --------------------------------------------
  issues_dt <- if (length(issue_log) > 0) {
    data.table::rbindlist(issue_log, fill = TRUE)
  } else {
    data.table::data.table(variable = character(), issue_type = character(), detail = character())
  }

  list(data = dt, issues = issues_dt)
}


# Internal helper: coerce a vector to a target type, returning NULL for
# unsupported types. Does NOT suppress NA warnings — that's the caller's job.
.safe_coerce <- function(x, target_type) {
  switch(
    target_type,
    "integer"   = suppressWarnings(as.integer(x)),
    "numeric"   = suppressWarnings(as.numeric(x)),
    "double"    = suppressWarnings(as.double(x)),
    "character" = as.character(x),
    "logical"   = suppressWarnings(as.logical(x)),
    NULL  # unknown type
  )
}


# Internal name normaliser — mirrors janitor::clean_names() behaviour for
# ASCII column names. Using janitor directly would require it as a hard
# dependency; keeping this internal makes the package lighter and avoids
# the need to import janitor at the namespace level.
# For full Unicode slug support (accented characters etc.) install janitor
# and call janitor::clean_names() instead.
.clean_names_dt <- function(df) {
  nms <- names(df)
  nms <- tolower(nms)
  nms <- gsub("[^a-z0-9]+", "_", nms)
  nms <- gsub("^_+|_+$", "", nms)
  # Ensure uniqueness (same as janitor)
  nms <- make.unique(nms, sep = "_")
  names(df) <- nms
  df
}
