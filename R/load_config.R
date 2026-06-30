#' Load and parse a variable map YAML configuration file
#'
#' Reads a YAML variable map and builds all lookup structures needed by the
#' cleaning pipeline: exact-name lookup, keyword patterns, type map, value
#' labels, recode tables, and missing-value declarations.
#'
#' @param config_file Path to the YAML variable map file.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{name_lookup}{Named character vector mapping aliases -> canonical names.}
#'   \item{label_lookup}{Named list of human-readable variable labels.}
#'   \item{keyword_patterns}{Named list of regex patterns for keyword matching.}
#'   \item{recode_map}{Named list of \code{data.table}s with raw->new value mappings.}
#'   \item{value_map}{Named list of \code{data.table}s with valid num/label pairs.}
#'   \item{missing_map}{Named list of vectors with values to be treated as missing.}
#'   \item{type_map}{Named character vector of target R types per variable.}
#'   \item{var_map}{The raw parsed YAML list (for downstream reference).}
#' }
#'
#' @examples
#' yml <- system.file("extdata", "variable_map.yml", package = "cuci")
#' config <- load_config(yml)
#' names(config$type_map)
#'
#' @importFrom yaml read_yaml
#' @importFrom data.table data.table
#' @export
load_config <- function(config_file = "config/variable_map.yml") {

  if (!file.exists(config_file)) {
    stop(sprintf("Config file not found: %s", config_file))
  }

  # Read with explicit UTF-8 encoding to handle Norwegian/special characters
  raw_lines <- readLines(config_file, encoding = "UTF-8", warn = FALSE)
  var_map   <- yaml::yaml.load(paste(raw_lines, collapse = "\n"))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # --- Exact name lookup: alias -> canonical ----------------------------
  # Include both raw YAML aliases AND their janitor-normalised forms so
  # that exact matching works regardless of whether clean_names() was applied.
  .normalise_name <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    gsub("^_+|_+$", "", x)
  }

  name_lookup <- lapply(names(var_map), function(canonical) {
    aliases <- var_map[[canonical]]$colnames
    if (is.null(aliases) || length(aliases) == 0) return(NULL)
    all_aliases <- unique(c(aliases, vapply(aliases, .normalise_name, character(1))))
    setNames(rep(canonical, length(all_aliases)), all_aliases)
  })
  name_lookup <- unlist(Filter(Negate(is.null), name_lookup))

  # --- Keyword pattern lookup: canonical -> regex -----------------------
  keyword_patterns <- build_keyword_patterns(list(var_map = var_map))

  # --- Type map: canonical -> R type ------------------------------------
  type_map <- sapply(names(var_map), function(canonical) {
    var_map[[canonical]]$type %||% "character"
  })

  # --- Label lookup: canonical -> description ---------------------------
  label_lookup <- lapply(names(var_map), function(canonical) {
    var_map[[canonical]]$label
  })
  names(label_lookup) <- names(var_map)

  # --- Value map: canonical -> data.table of integer_value/label --------
  # Only populated when the YAML `value:` block is non-null.
  value_map <- lapply(names(var_map), function(canonical) {
    value_list <- var_map[[canonical]]$value
    if (is.null(value_list)) return(NULL)
    data.table::data.table(
      num_value = as.integer(names(value_list)),
      chr_value = as.character(unlist(value_list))
    )
  })
  names(value_map) <- names(var_map)

  # --- Recode map: canonical -> data.table of raw->new values ----------
  recode_map <- lapply(names(var_map), function(canonical) {
    recode_list <- var_map[[canonical]]$recode
    if (is.null(recode_list)) return(NULL)
    data.table::data.table(
      raw_value = names(recode_list),
      new_value = as.integer(unlist(recode_list))
    )
  })
  names(recode_map) <- names(var_map)

  # --- Missing map: canonical -> vector of values treated as NA --------
  missing_map <- lapply(names(var_map), function(canonical) {
    miss <- var_map[[canonical]]$missing
    if (is.null(miss)) return(NULL)
    # Remove literal NA entries (YAML `~` or `NA`); keep numeric codes
    miss <- miss[!is.na(miss)]
    if (length(miss) == 0) return(NULL)
    miss
  })
  names(missing_map) <- names(var_map)

  list(
    name_lookup      = name_lookup,
    label_lookup     = label_lookup,
    keyword_patterns = keyword_patterns,
    recode_map       = recode_map,
    value_map        = value_map,
    missing_map      = missing_map,
    type_map         = type_map,
    var_map          = var_map
  )
}
