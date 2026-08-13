#' Apply Variable and Value Labels
#'
#' Apply variable labels and value labels to a data frame using a
#' configuration object.
#'
#' Variable labels are retrieved from `config$label_lookup`.
#' Value labels are retrieved from `config$value_map`.
#'
#' Variables without a value map will still receive a variable label
#' if one is defined in `config$label_lookup`.
#'
#' @param df A data frame.
#' @param config A configuration object containing:
#'   \describe{
#'     \item{label_lookup}{
#'       A named list of variable labels.
#'     }
#'     \item{value_map}{
#'       A named list of value mappings. Each mapping must contain
#'       columns `num_value` and `chr_value`.
#'     }
#'   }
#'
#' @return A data frame with variable labels and value labels applied.
#'
#' @examples
#' cfg <- list(
#'   label_lookup = list(
#'     kjonn = "IOs kjønn",
#'     alder = "IOs alder"
#'   ),
#'   value_map = list(
#'     kjonn = data.frame(
#'       num_value = c(1, 2),
#'       chr_value = c("Mann", "Kvinne")
#'     ),
#'     alder = NULL
#'   )
#' )
#'
#' df <- data.frame(
#'   kjonn = c(1, 2, 1),
#'   alder = c(25, 34, 41)
#' )
#'
#' df <- apply_labels(df, cfg)
#'
#' @export
apply_labels <- function(df, config) {

  for (var in names(config$label_lookup)) {

    if (!var %in% names(df)) {
      warning(sprintf("Variable '%s' not found in data frame.", var))
      next
    }

    var_label <- config$label_lookup[[var]]

    value_map <- config$value_map[[var]]

    # Variable label only
    if (is.null(value_map)) {

      if (!is.null(var_label)) {
        attr(df[[var]], "label") <- var_label
      }

      next
    }

    if (!is.numeric(df[[var]]) &&
          !is.integer(df[[var]])) {
      stop(
        sprintf(
          "Variable '%s' must be numeric when value labels are defined.",
          var
        ),
        call. = FALSE
      )
    }

    # Value labels
    labels <- stats::setNames(
      object = value_map$num_value,
      nm = value_map$chr_value
    )

    df[[var]] <- haven::labelled(
      x = df[[var]],
      labels = labels,
      label = var_label
    )
  }

  df
}
