
#' Count observations and calculate percentages
#'
#' Counts the number of observations for a variable and calculates the
#' corresponding percentages. Optionally converts the grouping variable
#' to a factor using labels defined in the configuration object.
#'
#' The function expects a configuration object containing a
#' `value_map` element, where each variable name corresponds to a lookup
#' table with columns `num_value` and `chr_value`.
#'
#' @param dt A `data.table` or `data.frame`. If a `data.frame` is supplied,
#'   it is converted internally to a `data.table`.
#' @param var Variable to summarize. Unquoted column name.
#' @param config Configuration list containing `value_map`.
#' @param category Logical. If `TRUE`, convert the grouping variable to a
#'   factor using labels from `config$value_map`.
#' @param digits Number of decimal places used when rounding percentages.
#'
#' @return A `data.table` containing:
#' \describe{
#'   \item{var}{The grouping variable.}
#'   \item{n}{Number of observations.}
#'   \item{pct}{Percentage of observations.}
#' }
#'
#' @details
#' If `category = TRUE` and the variable is of class
#' `haven_labelled`, labels are extracted using
#' `haven::as_factor(levels = "labels")`.
#'
#' Otherwise, labels are taken from
#' `config$value_map[[var_name]]`, using:
#'
#' \itemize{
#'   \item `num_value` as factor levels
#'   \item `chr_value` as factor labels
#' }
#'
#' @examples
#' library(data.table)
#'
#' dt <- data.table(
#'   kjonn = c(1, 2, 1, 2, 1, 1, 2, 2, 1, 2)
#' )
#'
#' cfg <- list(
#'   value_map = list(
#'     kjonn = data.table(
#'       num_value = c(1, 2),
#'       chr_value = c("Mann", "Kvinne")
#'     )
#'   )
#' )
#'
#' make_sum(dt, kjonn, cfg)
#'
#' make_sum(dt, kjonn, cfg, category = TRUE)
#'
#' @export
make_sum <- function(dt,
                     var,
                     config,
                     category = FALSE,
                     digits = 1) {


  if (!inherits(dt, c("data.frame", "data.table"))) {
    stop(
      "`dt` must be a data.frame or data.table.",
      call. = FALSE
    )
  }

  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  var_name <- as.character(substitute(var))
  code <- data.table::copy(config$value_map[[var_name]])

  if (is.null(code)) {
    stop(
      sprintf(
        "No value_map found for variable '%s'.",
        var_name
      ),
      call. = FALSE
    )
  }

  res <- dt[, .(n = .N), by = var_name][, pct := round(100 * n / sum(n), digits)]

  if (isTRUE(category)) {

    if (inherits(dt[[var_name]], "haven_labelled")) {
      res[, (var_name) := haven::as_factor(get(var_name), levels = "labels")]
    } else {
      res[,(var_name) := factor(get(var_name),
                                levels = code$num_value,
                                labels = code$chr_value
                                )]
    }
  }

  res[]
}
