#' @keywords internal
#' 
#' @importFrom data.table data.table rbindlist fread fwrite
#' @importFrom data.table setorder setcolorder fcase copy
"_PACKAGE"

utils::globalVariables(c(
  ":=",
  ".",
  ".N",
  ".conf_rank",
  "N",
  "confidence",
  "dataset",
  "logged_at",
  "method",
  "needs_review",
  "year"
))
