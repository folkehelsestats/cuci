# =============================================================================
# pipeline.R
#
# Top-level orchestration for the cuci cleaning pipeline.
#
# Public API:
#   run_pipeline()      - process a file manifest through the full pipeline
#
# Internal helpers:
#   .read_raw_file()    - read CSV / TSV / XLSX into a data.table
#   .process_one_file() - clean + validate one file, return result list
#   .merge_datasets()   - stack cleaned data.tables into one master table
#   .save_output()      - write a data.table or list of data.tables to disk
# =============================================================================


# -----------------------------------------------------------------------------
# .read_raw_file()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Read a raw data file into a data.table
#'
#' Dispatches on file extension.  Supports `.csv`, `.tsv`, and `.xlsx`.
#' `readxl` is only loaded if an `.xlsx` file is actually requested.
#'
#' @param path Path to the raw data file.
#' @return A `data.table`.
.read_raw_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    "csv"  = data.table::fread(path),
    "tsv"  = data.table::fread(path, sep = "\t"),
    "xlsx" = {
      if (!requireNamespace("readxl", quietly = TRUE))
        stop("Package 'readxl' is needed for .xlsx files. ",
             "Install with: install.packages('readxl')")
      data.table::as.data.table(readxl::read_excel(path))
    },
    stop(sprintf("Unsupported file type: .%s", ext))
  )
}


# -----------------------------------------------------------------------------
# .process_one_file()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Run the full pipeline for a single raw data file
#'
#' Reads the file, runs column matching, exports the match log, cleans the
#' data, and validates the result.  Returns a list with the cleaned
#' `data.table` and the issues table from [clean_dataset()].
#'
#' @param path          Path to the raw data file.
#' @param year_tag      Integer year, or `NA` if already in the file.
#' @param config        Config object from [load_config()].
#' @param log_dir       Directory for audit logs.
#' @param apply_missing Passed to [clean_dataset()].
#'
#' @return A list: `data` (cleaned `data.table`), `issues` (`data.table`),
#'   `label` (filename used as dataset label).
.process_one_file <- function(path, year_tag, config, log_dir, apply_missing) {

  label <- basename(path)
  cat(sprintf("\n\u2500\u2500 Processing: %s %s\n", label, strrep("\u2500", 30)))

  raw_dt <- .read_raw_file(path)

  # Column matching runs on normalised names, mirroring what clean_dataset does
  normed_names <- names(.normalise_colnames(data.table::copy(
    data.table::as.data.table(raw_dt)
  )))
  match_result <- match_columns(normed_names, config)

  result <- clean_dataset(
    dt            = raw_dt,
    config        = config,
    year_tag      = if (is.na(year_tag)) NULL else as.integer(year_tag),
    dataset_label = label,
    apply_missing = apply_missing
  )

  export_match_log(
    match_result  = match_result,
    issues_dt     = result$issues,
    dataset_label = label,
    year_tag      = if (is.na(year_tag)) NA else as.integer(year_tag),
    log_dir       = log_dir,
    append_master = TRUE
  )

  validate_dataset(result$data, config, label)

  if (nrow(result$issues) > 0) {
    cat(sprintf("\n  Issues for %s:\n", label))
    for (j in seq_len(nrow(result$issues)))
      cat(sprintf("    [%s] %s: %s\n",
                  result$issues$issue_type[j],
                  result$issues$variable[j],
                  result$issues$detail[j]))
  }

  list(data = result$data, issues = result$issues, label = label)
}


# -----------------------------------------------------------------------------
# .merge_datasets()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Stack a list of cleaned data.tables into one master data.table
#'
#' Uses [data.table::rbindlist()] with `fill = TRUE` so datasets with
#' different column sets are padded with `NA`.  The `year` column, if present,
#' is moved to the first position.
#'
#' @param clean_list List of `data.table`s (one per processed file).
#' @return A single stacked `data.table`.
.merge_datasets <- function(clean_list) {
  master_dt  <- data.table::rbindlist(clean_list, fill = TRUE, use.names = TRUE)
  other_cols <- setdiff(names(master_dt), "year")

  if ("year" %in% names(master_dt))
    data.table::setcolorder(master_dt, c("year", other_cols))

  master_dt
}


# -----------------------------------------------------------------------------
# .save_output()
# -----------------------------------------------------------------------------
#' @keywords internal
#' Write the pipeline output to disk
#'
#' Handles both the merged (`merge = TRUE`) and separate (`merge = FALSE`)
#' cases:
#' - Merged: writes a single CSV to `output_path`.
#' - Separate: writes one CSV per dataset, using the dataset label as the
#'   filename stem, into the directory given by `output_path`.
#'
#' Does nothing when `output_path` is `NULL`.
#'
#' @param output       Either a single `data.table` (merged) or a named list
#'   of `data.table`s (separate).
#' @param output_path  File path (merged) or directory path (separate), or
#'   `NULL` to skip writing.
#' @param merge        Logical - matches the `merge` argument from
#'   [run_pipeline()].
.save_output <- function(output, output_path, merge) {
  if (is.null(output_path)) return(invisible(NULL))

  if (merge) {
    # Single merged CSV
    out_dir <- dirname(output_path)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    data.table::fwrite(output, output_path)
    cat(sprintf(
      "\n\u2714 Master dataset saved: %s\n  %d rows | columns: %s\n",
      output_path, nrow(output), paste(names(output), collapse = ", ")
    ))
  } else {
    # One CSV per dataset, written into output_path as a directory
    if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
    for (label in names(output)) {
      safe  <- gsub("[^A-Za-z0-9_-]", "_", label)
      fpath <- file.path(output_path, sprintf("%s_clean.csv", safe))
      data.table::fwrite(output[[label]], fpath)
      cat(sprintf(
        "\n\u2714 Dataset saved: %s\n  %d rows | columns: %s\n",
        fpath, nrow(output[[label]]), paste(names(output[[label]]), collapse = ", ")
      ))
    }
  }

  invisible(NULL)
}


# -----------------------------------------------------------------------------
# run_pipeline()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Run the full survey data cleaning pipeline
#'
#' Processes every file in `file_manifest` through the complete pipeline
#' (read \u2192 match \u2192 log \u2192 clean \u2192 validate) and returns the results either
#' merged into one `data.table` or as a named list of separate `data.table`s.
#'
#' @section Output modes:
#' - `merge = TRUE` (default `FALSE`): stacks all cleaned datasets row-wise
#'   into a single `data.table` using [data.table::rbindlist()].  Missing
#'   columns across datasets are padded with `NA`.  `output_path` should be a
#'   full file path (e.g. `"data/clean/master.csv"`).
#' - `merge = FALSE`: returns a named list where each element is the cleaned
#'   `data.table` for one file, keyed by the file's basename.  `output_path`
#'   should be a directory (e.g. `"data/clean"`); one CSV per dataset is
#'   written there.
#'
#' @param file_manifest A `data.table` or data.frame with columns:
#'   \describe{
#'     \item{path}{Path to each raw input file (CSV, TSV, or XLSX).}
#'     \item{year_tag}{Integer year to attach, or `NA` if already in the file.}
#'   }
#' @param config_file   Path to the YAML variable map. Default
#'   `"config/variable_map.yml"`.
#' @param log_dir       Directory for audit logs. Default `"logs/matching"`.
#' @param output_path   Where to write the output.  A file path when
#'   `merge = TRUE`; a directory path when `merge = FALSE`.  Set `NULL` to
#'   skip writing entirely.
#' @param apply_missing Passed to [clean_dataset()]. Default `FALSE`.
#' @param merge         Logical. If `TRUE`, stack all cleaned datasets into one
#'   master `data.table`. If `FALSE` (default), return a named list of
#'   separate `data.table`s, one per input file.
#'
#' @return
#' - When `merge = TRUE`: a single `data.table` with `year` as the first
#'   column.
#' - When `merge = FALSE`: a named list of `data.table`s, one per file,
#'   keyed by the file's basename.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' manifest <- data.table(
#'   path     = c(
#'     system.file("extdata", "survey_2022.csv", package = "cuci"),
#'     system.file("extdata", "survey_2023.csv", package = "cuci")
#'   ),
#'   year_tag = c(2022L, 2023L)
#' )
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#'
#' # Return separate datasets (default)
#' result_list <- run_pipeline(manifest, config_file = yml, merge = FALSE)
#' result_list[["survey_2022.csv"]]
#'
#' # Return one merged dataset
#' result_merged <- run_pipeline(manifest, config_file = yml, merge = TRUE,
#'                               output_path = "data/clean/master.csv")
#' }
#'
#' @importFrom data.table data.table rbindlist fwrite setcolorder as.data.table
#' @export
run_pipeline <- function(file_manifest,
                         config_file   = "config/variable_map.yml",
                         log_dir       = "logs/matching",
                         output_path   = NULL,
                         apply_missing = FALSE,
                         merge         = FALSE) {

  config     <- load_config(config_file)
  n          <- nrow(file_manifest)
  clean_list <- vector("list", n)

  for (i in seq_len(n)) {
    res <- .process_one_file(
      path          = file_manifest$path[i],
      year_tag      = file_manifest$year_tag[i],
      config        = config,
      log_dir       = log_dir,
      apply_missing = apply_missing
    )
    clean_list[[i]] <- res$data
    names(clean_list)[i] <- res$label
  }

  # Produce final output according to the merge argument
  output <- if (merge) {
    .merge_datasets(clean_list)
  } else {
    clean_list   # named list, one data.table per file
  }

  .save_output(output, output_path, merge)
  summarise_master_log(log_dir)

  invisible(output)
}
