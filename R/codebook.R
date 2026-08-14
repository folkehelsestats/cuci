
#' Create an interactive codebook using DT
#'
#' This function generates an interactive HTML table (codebook) for a dataset,
#' showing variable names, labels, and value labels extracted from attributes.
#'
#' @param d A data frame, typically imported with `haven` or similar, where
#'   variables have `label` and `labels` attributes.
#' @param config Path to a YAML config file. Needed if dataset has not been
#'   imported with `haven` and value labels are not stored as attributes.
#' @param page Integer. Number of rows to display per page in the interactive
#'   table. Default is 25.
#' @param save Logical value. Default is FALSE
#' @return A `DT::datatable` object that can be rendered in R Markdown or Shiny
#'   or a stand alone HTML file.
#'
#' @details
#' The value labels are displayed on separate lines using HTML `<br>` tags.
#' To ensure these tags are rendered correctly, the argument `escape = FALSE`
#' is passed to `DT::datatable()`. Without this, HTML tags would be shown as
#' plain text instead of creating line breaks.
#'
#' The table includes:
#' \itemize{
#'   \item Column filters for easy searching.
#'   \item Export buttons (Copy, CSV, Excel, PDF, Print).
#' }
#'
#' @examples
#' \dontrun{
#' library(haven)
#' df <- read_dta("yourfile.dta")
#' make_codebook(df)
#' }
#'
#' @export
make_codebook <- function(d, config = NULL, page = 25, save = FALSE) {

  hvn <- any(vapply(d,
    function(x) inherits(x, "haven_labelled"),
    logical(1)
  ))

  if (isFALSE(hvn) & is.null(config)) {
    stop("Dataset does not have haven attributes. Please provide config file.",
         call. = FALSE)
  }

  if (!is.null(config)) {
    cfg <- load_config(config_file = config)
    d   <- apply_labels(d, cfg, strict = TRUE)
  }
  
  df <- extract_attr(d)

  code <- DT::datatable(df, filter = "top",
                        escape = FALSE,
                        extensions = 'Buttons',
                        rownames = FALSE,
                        options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          pageLength = page
                        ))

  if (save){
    htmlwidgets::saveWidget(code,
                            file = paste0(deparse(substitute(d)), ".html"),
                            selfcontained = TRUE)
  } else {
    return(code)
  }
}


#' @export
#' @rdname make_codebook 
codebook <- make_codebook


#' Extract variable metadata from a dataset
#'
#' This helper function extracts variable names, labels, and value labels
#' from a dataset where these are stored as attributes (e.g., `haven` imported data).
#'
#' @param d A data frame with attributes `label` and `labels` for each variable.
#'
#' @return A data.table with columns:
#' \describe{
#'   \item{name}{Variable name}
#'   \item{label}{Variable label (question text)}
#'   \item{value_labels}{Value labels formatted with HTML line breaks}
#' }
#'
#' @details
#' The function converts value labels into a single string separated by `<br>`
#' tags for proper display in HTML tables. For example:
#' \code{c("1 Yes", "2 No")} becomes \code{"1 Yes<br>2 No"}.
#'
#' @examples
#' \dontrun{
#' df <- read_spss("yourfile.sav")
#' extract_attr(df)
#' }
#'
#' @keywords internal
extract_attr <- function(d) {
  vars <- names(d)
  labels <- sapply(d, function(x) attr(x, "label"))
  value_labels <- sapply(d, function(x) {
    lbls <- attr(x, "labels")
    if (!is.null(lbls)) {
      paste0(lbls, " ", names(lbls), collapse = "<br>")
    } else {
      ""
    }
  })

  data.table::data.table(name = vars,
                         label = labels,
                         value_labels = value_labels
                         )
}
