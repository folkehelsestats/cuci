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
#' @param output_file Character string. Filename for the rendered HTML
#'   report. If `NULL`, a timestamped filename is generated automatically.
#'
#' @param open_browser Logical. If `TRUE`, the rendered report is opened
#'   in the default web browser upon successful rendering. Defaults to
#'   `interactive()`.
#'
#' @param create_dir Logical. Controls what happens if `output_dir` does
#'   not exist. If `TRUE`, the directory is created automatically. If
#'   `FALSE`, an error is thrown. The default is `interactive()`, which
#'   asks for confirmation in an interactive session and throws an error
#'   otherwise.
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
render_report <- function(
                          log_dir = NULL,
                          output_dir = NULL,
                          output_file = NULL,
                          open_browser = interactive(),
                          create_dir = interactive()
                          ){

  tmp <- withr::local_tempdir()
  src <- system.file("report", package = "cuci")

  if (src == "") {
    stop(
      "Could not locate the bundled report template.",
      call. = FALSE
    )
  }

  tmpdir <- fs::path(tmp, "report")
  fs::dir_copy(src, tmpdir, overwrite = TRUE)

  # ---- Validate master log exists ------------------------------------
  if (is.null(log_dir)) {
    stop("Please specify the 'log_dir' argument where logs files are located.",
         call. = FALSE)
  }
  
  master_path <- file.path(log_dir, "match_log_MASTER.csv")
  if (!file.exists(master_path)) {
    stop(sprintf(
      "Cannot render report: master log not found at '%s'.\nRun the cleaning pipeline first.",
      master_path
    ), call. = FALSE)
  }

  # ---- Determine output directory ---------------------------------------

  if (is.null(output_dir)) {
    output_dir <- fs::path_home("Documents")

    if (!fs::dir_exists(output_dir)) {
      output_dir <- fs::path_home()
    }
  }

  output_dir <- fs::path_norm(output_dir)

  # ---- Ensure output directory exists -----------------------------------

  if (!fs::dir_exists(output_dir)) {

    if (isTRUE(create_dir)) {

      fs::dir_create(output_dir, recurse = TRUE)
      message("Created output directory: ", output_dir)

    } else if (interactive()) {

      create <- utils::askYesNo(
        sprintf(
          "The output directory\n\n%s\n\ndoes not exist.\nCreate it?",
          output_dir
        )
      )

      if (isTRUE(create)) {
        fs::dir_create(output_dir, recurse = TRUE)
        message("Created output directory: ", output_dir)
      } else {
        stop("Report rendering cancelled.", call. = FALSE)
      }

    } else {

      stop(
        "Output directory does not exist: ",
        output_dir,
        "\nSpecify an existing directory or set create_dir = TRUE.",
        call. = FALSE
      )

    }
  }


  # ---- Build output filename (NO path — filename only) ---------------
  # Quarto requires output_file to be a bare filename with no directory
  # component. We pass the directory separately via output_dir.
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    output_file <- sprintf("report_%s.html", timestamp) # ← filename only
  }

  cat("\n── Rendering audit report ──────────────────────────────────\n")
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
    input          = file.path(tmpdir, "audit_report.qmd"),
    output_file    = output_file,          # ← plain filename, no slashes
    execute_dir    = getwd(),              # ← project root as working dir
    execute_params = list(log_dir = log_dir),
    quiet          = FALSE
  )

  # Copy HTML to user's destination
  rendered_file <- file.path(tmpdir, output_file)

  if (!fs::file_exists(rendered_file)) {
    stop(
      "Quarto completed but the rendered report was not found:\n",
      rendered_file,
      call. = FALSE
    )
  }

  fs::file_copy(
    rendered_file,
    file.path(output_dir, output_file),
    overwrite = TRUE
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
