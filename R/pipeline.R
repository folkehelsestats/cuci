#' Run the full survey data cleaning pipeline
#'
#' Convenience wrapper that processes one or more raw data files through the
#' complete pipeline: load config \u2192 read file(s) \u2192 match columns \u2192
#' export log \u2192 clean (recode, coerce, validate values) \u2192 validate structure.
#'
#' @param file_manifest A `data.table` (or data.frame) with columns:
#'   \describe{
#'     \item{path}{Path to each raw input file (CSV, TSV, or XLSX).}
#'     \item{year_tag}{Integer year to attach as a `year` column, or `NA` if
#'       the file already contains a year column.}
#'   }
#' @param config_file Path to the YAML variable map. Passed to [load_config()].
#' @param log_dir Directory for audit logs. Default `"logs/matching"`.
#' @param output_path Optional file path to write the stacked master CSV.
#'   Set `NULL` to skip writing.
#' @param apply_missing Passed to [clean_dataset()]. Default `FALSE`.
#'
#' @return A `data.table` containing all cleaned datasets stacked row-wise,
#'   with `year` as the first column.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' manifest <- data.table(
#'   path     = system.file("extdata", "test-data.csv", package = "cuci"),
#'   year_tag = 2023L
#' )
#' result <- run_pipeline(manifest, config_file = system.file(
#'   "extdata", "variable_map.yml", package = "cuci"
#' ))
#' result
#' }
#'
#' @importFrom data.table data.table rbindlist fwrite setcolorder as.data.table
#' @export
run_pipeline <- function(file_manifest,
                         config_file   = "config/variable_map.yml",
                         log_dir       = "logs/matching",
                         output_path   = "data/clean/master_clean.csv",
                         apply_missing = FALSE) {

  config     <- load_config(config_file)
  clean_list <- vector("list", nrow(file_manifest))

  for (i in seq_len(nrow(file_manifest))) {

    path     <- file_manifest$path[i]
    year_tag <- file_manifest$year_tag[i]
    label    <- basename(path)

    cat(sprintf("\n\u2500\u2500 Processing: %s %s\n", label, strrep("\u2500", 30)))

    raw_dt <- .read_raw_file(path)

    # Run matching on normalised names (mirrors what clean_dataset does internally)
    normed_names <- names(
      data.table::as.data.table(janitor::clean_names(as.data.frame(raw_dt)))
    )
    match_result <- match_columns(normed_names, config)

    export_match_log(
      match_result  = match_result,
      dataset_label = label,
      year_tag      = if (is.na(year_tag)) NA else year_tag,
      log_dir       = log_dir,
      append_master = TRUE
    )

    result <- clean_dataset(
      dt            = raw_dt,
      config        = config,
      year_tag      = if (is.na(year_tag)) NULL else year_tag,
      dataset_label = label,
      apply_missing = apply_missing
    )

    validate_dataset(result$data, config, label)

    if (nrow(result$issues) > 0) {
      cat(sprintf("\n  Issues for %s:\n", label))
      for (j in seq_len(nrow(result$issues))) {
        cat(sprintf("    [%s] %s: %s\n",
            result$issues$issue_type[j],
            result$issues$variable[j],
            result$issues$detail[j]))
      }
    }

    clean_list[[i]] <- result$data
  }

  # Stack all cleaned datasets
  master_dt  <- data.table::rbindlist(clean_list, fill = TRUE, use.names = TRUE)
  other_cols <- setdiff(names(master_dt), "year")

  if ("year" %in% names(master_dt)) {
    data.table::setcolorder(master_dt, c("year", other_cols))
  }

  if (!is.null(output_path)) {
    out_dir <- dirname(output_path)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    data.table::fwrite(master_dt, output_path)
    cat(sprintf(
      "\n\u2714 Master dataset saved: %s\n  %d rows | columns: %s\n",
      output_path, nrow(master_dt), paste(names(master_dt), collapse = ", ")
    ))
  }

  summarise_master_log(log_dir)

  invisible(master_dt)
}


# Internal: read a raw file by extension into a data.table
.read_raw_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    "csv"  = data.table::fread(path),
    "tsv"  = data.table::fread(path, sep = "\t"),
    "xlsx" = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read .xlsx files. Install it with install.packages('readxl').")
      }
      data.table::as.data.table(readxl::read_excel(path))
    },
    stop(sprintf("Unsupported file type: .%s", ext))
  )
}
