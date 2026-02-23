#' Redact case ID patterns from free text
#'
#' Removes case IDs that appear verbatim in the free-text column using a
#' regex pattern. The default pattern covers the standard German LIS format:
#' `AK/`, `A/`, `EK/`, or `E/` followed by 1-6 digits, a slash, and 1-2
#' digits (e.g. `AK/123456/24`, `E/999/1`).
#'
#' Run [setup_ner()] before calling this function.
#'
#' @param df A tibble, typically output from [deidentify_names()] or
#'   [deidentify_names_dict()].
#' @param text_col Name of the text column. Defaults to `"text"`.
#' @param replacement Placeholder for redacted IDs. Defaults to
#'   `"[FALL-ID]"`.
#' @param pattern Custom regex pattern as a string. If `NULL` (default),
#'   uses the built-in pattern `\b(?:AK|EK|A|E)/\d{1,6}/\d{1,2}\b`.
#'   Supply your own if your LIS uses a different format.
#' @param verbose Print a redaction summary. Default `TRUE`.
#'
#' @return The input tibble with case IDs redacted in `text_col`, plus a
#'   new integer column `n_case_ids_redacted`.
#'
#' @export
#' @examples
#' \dontrun{
#' setup_ner()
#'
#' # Default pattern
#' clean <- deidentify_case_ids(clean)
#'
#' # Custom pattern
#' clean <- deidentify_case_ids(clean, pattern = r"(\bP-\d{6}/\d{2}\b)")
#' }
deidentify_case_ids <- function(df,
                                text_col    = "text",
                                replacement = "[FALL-ID]",
                                pattern     = NULL,
                                verbose     = TRUE) {

  if (!text_col %in% names(df)) {
    rlang::abort(paste0("Column '", text_col, "' not found."))
  }

  if (!reticulate::py_has_attr(reticulate::py, "redact_case_ids_batch")) {
    rlang::abort(
      "redact_case_ids_batch() not found. Did you call setup_ner() first?"
    )
  }

  original <- df[[text_col]]

  redacted <- reticulate::py$redact_case_ids_batch(
    as.list(original),
    replacement = replacement,
    pattern     = if (is.null(pattern)) reticulate::py_none() else pattern
  )
  redacted <- unlist(redacted)

  n_redacted <- pmax(0L,
    as.integer(stringr::str_count(redacted, stringr::fixed(replacement))) -
    as.integer(stringr::str_count(original, stringr::fixed(replacement)))
  )

  df[[text_col]]                <- redacted
  df[["n_case_ids_redacted"]]   <- n_redacted

  if (verbose) {
    total <- sum(n_redacted, na.rm = TRUE)
    rows  <- sum(n_redacted > 0, na.rm = TRUE)
    message("Case IDs: redacted ", total, " ID(s) across ", rows, " row(s).")
  }

  df
}
