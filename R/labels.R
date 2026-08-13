#' Apply Variable and Value Labels
#'
#' Apply variable labels and value labels to a data frame using a
#' configuration object.
#'
#' Variable labels are retrieved from `config$label_lookup`.
#' Value labels are retrieved from `config$value_map`.
#'
#' Variables without a value map will still receive a variable label if
#' one is defined in `config$label_lookup`.
#'
#' When `strict = TRUE`, variables containing values not defined in
#' the corresponding value map will trigger an error.
#'
#' When `strict = FALSE`, values not defined in the value map are
#' converted to `NA` and processing continues.
#'
#' @param df A data frame.
#' @param config A configuration object containing:
#'   \describe{
#'     \item{label_lookup}{
#'       Named list of variable labels.
#'     }
#'     \item{value_map}{
#'       Named list of value-label mappings. Each mapping must contain
#'       columns \code{num_value} and \code{chr_value}.
#'     }
#'   }
#' @param strict Logical. If \code{TRUE}, variables containing values
#'   not present in \code{value_map$num_value} will trigger an error.
#'   If \code{FALSE}, invalid values are converted to \code{NA}.
#'
#' @return A data frame with variable labels and value labels applied.
#'
#' @examples
#' cfg <- list(
#'   label_lookup = list(
#'     kjonn = "IOs kjønn"
#'   ),
#'   value_map = list(
#'     kjonn = data.frame(
#'       num_value = c(1L, 2L),
#'       chr_value = c("Mann", "Kvinne")
#'     )
#'   )
#' )
#'
#' df <- data.frame(
#'   kjonn = c("1", "2", "D")
#' )
#'
#' apply_labels(df, cfg, strict = FALSE)
#'
#' @export
apply_labels <- function(df,
                         config,
                         strict = TRUE) {

  for (var in names(config$label_lookup)) {

    if (!var %in% names(df)) {
      warning(
        sprintf(
          "Variable '%s' not found in data frame.",
          var
        ),
        call. = FALSE
      )
      next
    }

    var_label <- config$label_lookup[[var]]
    value_map <- config$value_map[[var]]

    # Variable label only
    if (is.null(value_map)) {

      attr(df[[var]], "label") <- var_label

      next
    }

    valid_codes <- as.character(value_map$num_value)
    data_values <- as.character(df[[var]])

    invalid_idx <- !is.na(data_values) &
      !data_values %in% valid_codes

    # Strict validation
    if (isTRUE(strict) && any(invalid_idx)) {

      invalid_values <- unique(
        data_values[invalid_idx]
      )

      stop(
        sprintf(
          "Variable '%s' contains values not defined in value_map: %s",
          var,
          paste(invalid_values, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Non-strict mode
    if (isFALSE(strict) && any(invalid_idx)) {

      warning(
        sprintf(
          "Variable '%s': %d value(s) converted to NA.",
          var,
          sum(invalid_idx)
        ),
        call. = FALSE
      )

      data_values[invalid_idx] <- NA_character_
    }

    # Convert to same type as num_value
    if (is.integer(value_map$num_value)) {

      df[[var]] <- suppressWarnings(
        as.integer(data_values)
      )

    } else if (is.double(value_map$num_value)) {

      df[[var]] <- suppressWarnings(
        as.numeric(data_values)
      )

    } else if (is.character(value_map$num_value)) {

      df[[var]] <- data_values

    } else {

      stop(
        sprintf(
          "Unsupported label type for variable '%s'.",
          var
        ),
        call. = FALSE
      )
    }

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
