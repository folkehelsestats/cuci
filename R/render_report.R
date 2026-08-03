#' Render Report from Matching Logs
#'
#' Render a Quarto-based HTML audit report summarizing the results of a
#' column-matching workflow.
#'
#' The report is generated from the bundled Quarto template
#' (`audit_report.qmd`) included with the package and uses the log files
#' located in `log_dir`.
#'
#' A master log file named `match_log_MASTER.csv` must exist in
#' `log_dir`; otherwise the report cannot be rendered.
#'
#' During rendering, the report is executed with
#' `execute_dir = getwd()`, allowing relative paths in the Quarto
#' document (for example `logs/matching`) to be resolved relative to the
#' current project directory.
#'
#' @param log_dir Character string. Directory containing matching log
#'   files. Must include `match_log_MASTER.csv`.
#'
#' @param output_dir Character string. Directory where the rendered HTML
#'   report will be written.
#'
#' @param open_browser Logical. If `TRUE`, the rendered report is opened
#'   in the default web browser upon successful rendering. Defaults to
#'   `interactive()`.
#'
#' @return Invisibly returns the path where the rendered report is
#'   expected to be written.
#'
#' @details
#' The report template (`audit_report.qmd`) and associated assets are
#' distributed with the package under `inst/quarto/`.
#'
#' Quarto requires `output_file` to be a plain filename rather than a
#' path. For this reason a timestamped filename is generated during
#' rendering.
#'
#' @examples
#' \dontrun{
#' render_audit_report()
#'
#' render_audit_report(
#'   log_dir = "logs/matching",
#'   output_dir = "reports"
#' )
#' }
#'
#' @seealso [quarto::quarto_render()]
#'
#' @export
render_report <- function(log_dir      = NULL,
                          output_dir   = "reports",
                          open_browser = interactive()
                          ) {

  qmd_file <- system.file(
    "quarto",
    "audit_report.qmd",
    package = "cuci"
  )

  # ---- Validate master log exists ------------------------------------
  master_path <- file.path(log_dir, "match_log_MASTER.csv")
  if (!file.exists(master_path)) {
    stop(sprintf(
      "Cannot render report: master log not found at '%s'.\nRun the cleaning pipeline first.",
      master_path
    ), call. = FALSE)
  }

  # ---- Ensure output directory exists --------------------------------
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(sprintf("Created output directory: %s", output_dir))
  }

  # ---- Locate and validate Quarto template ie. the .qmd template -----
  if (!file.exists(qmd_file)) {
    stop(sprintf("Quarto template not found at '%s'.", qmd_file), call. = FALSE)
  }

  # ---- Build output filename (NO path — filename only) ---------------
  # Quarto requires output_file to be a bare filename with no directory
  # component. We pass the directory separately via output_dir.
  timestamp   <- format(Sys.time(), "%Y%m%d_%H%M")
  output_file <- sprintf("audit_report_%s.html", timestamp)   # ← filename only

  cat("\n── Rendering audit report ──────────────────────────────────\n")
  cat(sprintf("  Template   : %s\n", qmd_file))
  cat(sprintf("  Log dir    : %s\n", log_dir))
  cat(sprintf("  Output dir : %s\n", output_dir))
  cat(sprintf("  File       : %s\n", output_file))
  cat("  Please wait...\n\n")
  
  # ---- Render --------------------------------------------------------
  # `output_file`  = bare filename (Quarto constraint on Windows + all OS)
  # `output_dir`   = where to write it (Quarto moves it here after render) .. quarto has no option to specify output_dir anymore
  # `execute_dir`  = working directory for the .qmd execution;
  #                  set to the project root so relative paths in the
  #                  .qmd (e.g. logs/matching) resolve correctly
  quarto::quarto_render(
    input          = qmd_file,
    output_file    = output_file,          # ← plain filename, no slashes
    execute_dir    = getwd(),              # ← project root as working dir
    execute_params = list(log_dir = log_dir),
    quiet          = FALSE
  )

  # ---- Confirm and optionally open -----------------------------------
  final_path <- file.path(output_dir, output_file)

  if (file.exists(final_path)) {
    size_kb <- round(file.size(final_path) / 1024, 1)
    cat(sprintf("\n✔ Report rendered: %s (%.1f KB)\n", final_path, size_kb))
    if (open_browser) browseURL(final_path)
  } else {
    warning("Rendering completed but output file was not found at: ", final_path)
  }

  invisible(final_path)
}
