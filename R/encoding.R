#' Fix mis-encoded Norwegian characters
#'
#' Repairs strings where Norwegian letters (\eqn{\text{ae, AE, aa, AA, oe, OE}})
#' have been mangled by encoding mismatches — typically UTF-8 bytes misread as
#' Latin-1/Windows-1252 (double-encoding), or raw Latin-1 bytes read directly.
#' This is a common issue when reading survey data exported from systems with
#' inconsistent encoding handling.
#'
#' @param x A character vector to fix.
#'
#' @details
#' The function corrects two classes of problems, applied in sequence via
#' [Reduce()]:
#' \itemize{
#'   \item \strong{Double-encoded UTF-8}: e.g. the UTF-8 bytes for "ae"
#'     (\code{C3 A6}) get misread as Latin-1 and re-encoded as two separate
#'     characters, displaying as \code{"\u00c3\u00a6"}. These are mapped back
#'     to the correct single character.
#'   \item \strong{Raw Latin-1 / Windows-1252 bytes}: e.g. \code{"\xe6"} is
#'     mapped directly to \code{"\u00e6"}.
#' }
#'
#' \strong{Uppercase AE is a special case.} Its UTF-8 bytes are \code{C3 86}.
#' When double-encoded, \code{C3} becomes \code{"\u00c3"} as usual, but byte
#' \code{86} is interpreted differently depending on the assumed source
#' encoding: it is unassigned in strict ISO-8859-1, but decodes to
#' \code{"\u2020"} (dagger) under Windows-1252 — the encoding Windows tools
#' such as Excel typically use. Since this pipeline runs on Windows, the
#' function matches the Windows-1252 form (\code{"\u00c3\u2020"}). If you see
#' stray literal dagger characters appear in cleaned output, that is a sign
#' this assumption doesn't hold for your data source and the pattern should
#' be revisited.
#'
#' \strong{Important implementation note:} the double-encoded patterns use a
#' \emph{single} backslash Unicode escape (e.g. \code{"\u00c3\u00a5"}) so
#' that R evaluates them as the actual Unicode characters at parse time.
#' Using a double backslash (\code{"\\u00c3\\u00a5"}) is a common mistake —
#' it produces the literal six-character text \code{\\u00c3\\u00a5} instead
#' of the intended character, and the pattern will silently never match.
#'
#' R's parser also does not allow a \code{\\u} (Unicode) escape and a
#' \code{\\x} (hex) escape to appear together \emph{within the same string
#' literal} — this raises a "mixing Unicode and octal/hex escapes" parse
#' error. Because each pattern here needs both a Unicode-escaped form (for
#' the double-encoded case) and a hex-escaped raw byte (for the single-byte
#' case), each piece is written as its own string literal and joined at
#' runtime with \code{\link{paste0}}, which only concatenates already-parsed
#' strings and never triggers this restriction.
#'
#' Matching is done with \code{useBytes = TRUE} throughout, so replacement
#' operates on the raw byte sequences rather than relying on the (possibly
#' incorrect) declared encoding of \code{x}.
#'
#' @return A character vector of the same length as \code{x}, with corrected
#'   encoding.
#'
#' @references
#' Adapted from
#' \url{https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301}
#'
#' @examples
#' \dontrun{
#' fix_encode("Sm\xe5 tr\xf8ndersk kr\xe6mmerhus")
#' fix_encode("Sm\u00c3\u00a5 tr\u00c3\u00b8ndersk kr\u00c3\u00a6mmerhus")
#' fix_encode("\u00c3\u2020rsrapport") # double-encoded "AErsrapport"-style AE
#' }
#'
#' @keywords internal
#' @export
fix_encode <- function(x) {

  # Each pattern covers both the double-encoded UTF-8 form and the raw
  # Latin-1/Windows-1252 byte for the same character.
  #
  # NOTE: R does not allow \u (Unicode) and \x (hex) escapes to be mixed
  # within a single string literal, so the Unicode-escaped piece and the
  # hex-escaped piece are written as SEPARATE literals and joined with
  # paste0() at runtime. Each piece individually uses only one escape type.
  patterns <- c(
    paste0("\u00c3\u00a6", "|", "\xe6"), # ae
    paste0("\u00c3\u2020", "|", "\xc6"), # AE (Windows-1252 double-encoding)
    paste0("\u00c3\u00a5", "|", "\xe5"), # aa
    paste0("\u00c3\u00b8", "|", "\xf8"), # oe
    "\xed",                              # stray mis-decoded byte
    "\xc5",                              # AA
    "\xd8"                               # OE
  )

  values <- c(
    "\u00e6", # ae
    "\u00c6", # AE
    "\u00e5", # aa
    "\u00f8", # oe
    "i",      # stray mis-decoded byte -> plain i
    "\u00c5", # AA
    "\u00d8"  # OE
  )

  Reduce(
    function(acc, i) gsub(patterns[i], values[i], acc, useBytes = TRUE),
    seq_along(patterns),
    init = x
  )
}


# OBS! Stil not able to handle double-encoded AE (Å) correctly, so that is
# commented out above. It is not a problem for the data we have seen so far, but
# it may be a problem for other data sources.



#' Repair mis-encoded Norwegian characters
#'
#' Repairs strings where Norwegian letters (\eqn{\text{ae, AE, aa, AA, oe, OE}})
#' have been mangled by encoding mismatches — typically UTF-8 bytes misread as
#' Latin-1/Windows-1252 (double-encoding), or raw Latin-1 bytes read directly.
#' This is a common issue when reading survey data exported from systems with
#' inconsistent encoding handling.
#'
#' Encoding solution with some modification from
#' https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301
#'
#' This function is the based for the function \code{fix_encode} above, but it
#' is not exported and is not used in the package.
#' @param x A character vector to fix.
#' @return A character vector of the same length as \code{x}, with corrected
#' @keywords internal
repair_encode <- function(x) gsub("\\u00c3\\u00a6|\xe6", "\u00e6", useBytes = TRUE,
                              gsub("\\u00c3\\u00a5|\xe5", "\u00e5", useBytes = TRUE,
                                   gsub("\\u00c3\\u00b8|\xf8", "\u00f8", useBytes = TRUE,
                                        gsub("\xed", "i", useBytes = TRUE,
                                             gsub("\xc5", "\u00c5", useBytes = TRUE,
                                                  gsub("\xd8", "\u00d8", x, useBytes = TRUE))))))
