# =============================================================================
# load_config.R
#
# Parse a YAML variable map into all lookup structures the pipeline needs.
#
# Public API (exported):
#   load_config()         - orchestrator; calls all builders and returns config
#
# Internal helpers (not exported, prefixed with `.`):
#   .read_yaml_utf8()     - read YAML file safely with UTF-8 encoding
#   .normalise_alias()    - snake_case-normalise a single alias string
#   .build_name_lookup()  - alias  -> canonical  (exact match layer)
#   .build_type_map()     - canonical -> R type
#   .build_label_lookup() - canonical -> human-readable label
#   .build_value_map()    - canonical -> data.table of integer/label pairs
#   .build_recode_map()   - canonical -> data.table of raw/new value pairs
#   .build_missing_map()  - canonical -> vector of numeric missing codes
# =============================================================================


# Package-level null-coalescing operator used by several helpers
`%||%` <- function(a, b) if (!is.null(a)) a else b


# -----------------------------------------------------------------------------
# .read_yaml_utf8()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Read a YAML file with explicit UTF-8 encoding
#'
#' Using [yaml::read_yaml()] directly can fail on Windows when the file
#' contains non-ASCII characters (e.g. Norwegian letters) because R may pick
#' the wrong locale encoding.  Reading the raw lines first and passing the
#' collapsed string to [yaml::yaml.load()] bypasses this entirely.
#'
#' @param path Path to the YAML file.
#' @return The parsed YAML as a named R list.
.read_yaml_utf8 <- function(path) {
  if (!file.exists(path))
    stop(sprintf("Config file not found: %s", path))

  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  yaml::yaml.load(paste(lines, collapse = "\n"))
}


# -----------------------------------------------------------------------------
# .normalise_alias()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Normalise a single alias string to snake_case
#'
#' Mirrors the transformation applied by [.normalise_colnames()] in
#' `clean_dataset.R` so that aliases from the YAML still match after
#' `clean_names()` has been applied to the raw data.
#'
#' For example, `"IO_Kjonn"` becomes `"io_kjonn"`, so an exact match fires
#' even though the YAML only listed `"IO_Kjonn"`.
#'
#' @param x A single character string.
#' @return Normalised string.
.normalise_alias <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}


# -----------------------------------------------------------------------------
# .build_name_lookup()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Build the alias -> canonical name lookup vector
#'
#' For each canonical variable, reads its `colnames:` list from the YAML and
#' produces two entries per alias: the raw string as written and its
#' normalised (snake_case) form.  Both are mapped to the canonical name so
#' that exact matching works whether or not `clean_names()` has been applied.
#'
#' @param var_map Parsed YAML list.
#' @return Named character vector: names are aliases, values are canonicals.
.build_name_lookup <- function(var_map) {
  entries <- lapply(names(var_map), function(canonical) {
    aliases <- var_map[[canonical]]$colnames
    if (is.null(aliases) || length(aliases) == 0) return(NULL)

    # Both raw and normalised forms map to the same canonical
    all_aliases <- unique(c(aliases, vapply(aliases, .normalise_alias, character(1))))
    stats::setNames(rep(canonical, length(all_aliases)), all_aliases)
  })

  unlist(Filter(Negate(is.null), entries))
}


# -----------------------------------------------------------------------------
# .build_type_map()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Build the canonical -> R type map
#'
#' Reads the `type:` field for each variable.  Falls back to `"character"`
#' when the field is absent, so every variable always has a target type.
#'
#' @param var_map Parsed YAML list.
#' @return Named character vector: names are canonicals, values are R type
#'   strings ("integer", "numeric", "character", etc.).
.build_type_map <- function(var_map) {
  sapply(names(var_map), function(canonical) {
    var_map[[canonical]]$type %||% "character"
  })
}


# -----------------------------------------------------------------------------
# .build_label_lookup()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Build the canonical -> human-readable label lookup
#'
#' Reads the `label:` field for each variable.  Used in reports and audit
#' logs to display descriptive names rather than raw variable codes.
#'
#' @param var_map Parsed YAML list.
#' @return Named list: names are canonicals, values are label strings (or
#'   `NULL` if the field was absent).
.build_label_lookup <- function(var_map) {
  labels <- lapply(names(var_map), function(canonical) {
    var_map[[canonical]]$label
  })
  stats::setNames(labels, names(var_map))
}


# -----------------------------------------------------------------------------
# .build_value_map()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Build the canonical -> valid-value table map
#'
#' For each variable with a `value:` block, creates a two-column `data.table`
#' listing every declared valid integer code alongside its human-readable
#' label.  Variables without a `value:` block return `NULL`.
#'
#' The `num_value` column (integer) is used by [.validate_column_values()] to
#' check observed data; `chr_value` is used for labelling in reports.
#'
#' @param var_map Parsed YAML list.
#' @return Named list of `data.table`s (or `NULL` per variable).
.build_value_map <- function(var_map) {
  maps <- lapply(names(var_map), function(canonical) {
    value_list <- var_map[[canonical]]$value
    if (is.null(value_list)) return(NULL)

    data.table::data.table(
      num_value = as.integer(names(value_list)),
      chr_value = as.character(unlist(value_list))
    )
  })
  stats::setNames(maps, names(var_map))
}


# -----------------------------------------------------------------------------
# .build_recode_map()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Build the canonical -> recode table map
#'
#' For each variable with a `recode:` block, creates a two-column `data.table`
#' mapping raw string values to their standardised integer replacements.
#' Variables without a `recode:` block return `NULL`.
#'
#' Recoding operates on the string representation of column values so it is
#' type-independent and always runs before type coercion.
#'
#' @param var_map Parsed YAML list.
#' @return Named list of `data.table`s (or `NULL` per variable).
.build_recode_map <- function(var_map) {
  maps <- lapply(names(var_map), function(canonical) {
    recode_list <- var_map[[canonical]]$recode
    if (is.null(recode_list)) return(NULL)

    data.table::data.table(
      raw_value = names(recode_list),
      new_value = as.integer(unlist(recode_list))
    )
  })
  stats::setNames(maps, names(var_map))
}


# -----------------------------------------------------------------------------
# .build_missing_map()
# -----------------------------------------------------------------------------
#' @keywords internal
#'
#' Build the canonical -> missing-code vector map
#'
#' Reads the `missing:` block for each variable and returns only the numeric
#' codes (e.g. `8`, `9`).  Literal YAML `~` / `NA` entries are stripped here
#' because those cells are already `NA` in R and need no recoding.
#'
#' The result is used by [.apply_missing_codes()] when `apply_missing = TRUE`
#' is passed to [clean_dataset()].
#'
#' @param var_map Parsed YAML list.
#' @return Named list of integer vectors (or `NULL` per variable).
.build_missing_map <- function(var_map) {
  maps <- lapply(names(var_map), function(canonical) {
    miss <- var_map[[canonical]]$missing
    if (is.null(miss)) return(NULL)

    # Drop literal NA entries - only keep numeric codes
    miss <- miss[!is.na(miss)]
    if (length(miss) == 0) return(NULL)
    miss
  })
  stats::setNames(maps, names(var_map))
}


# -----------------------------------------------------------------------------
# load_config()   [PUBLIC]
# -----------------------------------------------------------------------------
#' Load and parse a variable map YAML configuration file
#'
#' Reads the YAML variable map and delegates each lookup-table construction
#' to a focused internal helper.  Returns a single config list that is passed
#' to all other pipeline functions.
#'
#' @param config_file Path to the YAML variable map file.
#'
#' @return A named list:
#' \describe{
#'   \item{name_lookup}{Named character vector: alias -> canonical.}
#'   \item{label_lookup}{Named list: canonical -> label string.}
#'   \item{keyword_patterns}{Named list: canonical -> regex string.}
#'   \item{recode_map}{Named list of `data.table`s: raw -> new value.}
#'   \item{value_map}{Named list of `data.table`s: integer code / label.}
#'   \item{missing_map}{Named list of numeric vectors: codes treated as NA.}
#'   \item{type_map}{Named character vector: canonical -> R type.}
#'   \item{var_map}{The raw parsed YAML list (for downstream reference).}
#' }
#'
#' @examples
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#' config <- load_config(yml)
#' names(config$type_map)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.table data.table
#' @export
load_config <- function(config_file = "config/variable_map.yml") {

  # 1. Read YAML safely (handles UTF-8 / Norwegian chars on Windows)
  var_map <- .read_yaml_utf8(config_file)

  # 2. Build each lookup structure via its own focused helper
  list(
    name_lookup      = .build_name_lookup(var_map),
    label_lookup     = .build_label_lookup(var_map),
    keyword_patterns = build_keyword_patterns(list(var_map = var_map)),
    recode_map       = .build_recode_map(var_map),
    value_map        = .build_value_map(var_map),
    missing_map      = .build_missing_map(var_map),
    type_map         = .build_type_map(var_map),
    var_map          = var_map
  )
}
