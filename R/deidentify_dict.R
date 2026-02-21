#' Redact physician surnames using a German surname dictionary
#'
#' An optional complement to [deidentify_names()] that catches names the NER
#' models may miss. Matches whole words case-insensitively against a list of
#' common German surnames. By default, also extends matches backwards to
#' include preceding medical titles (Dr., Prof., Doz., OA, CA, etc.).
#'
#' Because dictionary matching has no semantic context, it will produce false
#' positives for common surnames that also appear as regular words (e.g.
#' "Koch", "Wolf", "Winter"). Review `n_dict_redacted` in the output to
#' assess the impact and adjust `min_length` or provide a custom dictionary
#' to reduce noise.
#'
#' Call [setup_ner()] before using this function (the Python session must be
#' active, even though this function uses pure regex matching).
#'
#' @param df A tibble, typically the output of [deidentify_names()].
#' @param text_col Name of the text column. Defaults to `"text"`.
#' @param replacement Placeholder for redacted spans. Defaults to `"[NAME]"`.
#' @param dict_path Path to a plain-text surname file (one name per line,
#'   `#` for comments). Defaults to the built-in German surname list shipped
#'   with the package (`inst/data/german_surnames.txt`).
#' @param title_prefix If `TRUE` (default), extends matches to include
#'   preceding medical titles: `Dr.`, `Prof.`, `Doz.`, `OA`, `CA`, etc.
#' @param min_length Minimum surname length to match. Defaults to `4` to
#'   avoid redacting very short common words (e.g. "Ott", "Leu").
#' @param custom_surnames Optional character vector of additional surnames to
#'   add to the dictionary for this call (e.g. names from your own department).
#' @param verbose Print a redaction summary. Default `TRUE`.
#'
#' @return The input tibble with additional dictionary redactions applied to
#'   `text_col`, plus a new integer column `n_dict_redacted`.
#'
#' @export
#' @examples
#' \dontrun{
#' setup_ner()
#' clean <- df |>
#'   deidentify_names() |>
#'   deidentify_names_dict()
#'
#' # Add department-specific names and lower min_length
#' clean <- deidentify_names_dict(clean,
#'   custom_surnames = c("Maas", "Loh"),
#'   min_length = 3)
#' }
deidentify_names_dict <- function(df,
                                  text_col         = "text",
                                  replacement      = "[NAME]",
                                  dict_path        = NULL,
                                  title_prefix     = TRUE,
                                  min_length       = 4L,
                                  custom_surnames  = NULL,
                                  verbose          = TRUE) {

  if (!text_col %in% names(df)) {
    rlang::abort(paste0("Column '", text_col, "' not found."))
  }

  if (!reticulate::py_has_attr(reticulate::py, "redact_names_dict_batch")) {
    rlang::abort(
      "redact_names_dict_batch() not found. Did you call setup_ner() first?"
    )
  }

  # ── Resolve dictionary path ───────────────────────────────────────────────
  if (is.null(dict_path)) {
    dict_path <- system.file("data", "german_surnames.txt", package = "lisanon")
    if (!nchar(dict_path)) {
      rlang::abort("Built-in surname dictionary not found. Is lisanon installed correctly?")
    }
  } else {
    if (!file.exists(dict_path)) {
      rlang::abort(paste0("Dictionary file not found: ", dict_path))
    }
  }

  # ── Apply min_length and custom surnames via a temp file ──────────────────
  # Read the base dictionary, apply min_length filter, append custom names
  base_names <- readLines(dict_path, encoding = "UTF-8", warn = FALSE)
  base_names <- base_names[!grepl("^#", base_names) & nchar(trimws(base_names)) > 0]
  base_names <- base_names[nchar(base_names) >= min_length]

  if (!is.null(custom_surnames)) {
    custom_surnames <- custom_surnames[nchar(custom_surnames) >= min_length]
    base_names <- unique(c(base_names, custom_surnames))
  }

  # Write filtered list to a temp file for Python to read
  tmp_dict <- tempfile(fileext = ".txt")
  writeLines(base_names, tmp_dict, useBytes = FALSE)
  on.exit(unlink(tmp_dict), add = TRUE)

  # ── Run redaction ─────────────────────────────────────────────────────────
  original <- df[[text_col]]

  message("Running dictionary NER on ", length(original), " texts (",
          length(base_names), " surnames)...")

  redacted <- reticulate::py$redact_names_dict_batch(
    as.list(original),
    dict_path    = tmp_dict,
    replacement  = replacement,
    title_prefix = title_prefix
  )
  redacted <- unlist(redacted)

  # Count new redactions (beyond those already in the text)
  n_new <- pmax(0L,
    as.integer(stringr::str_count(redacted,  stringr::fixed(replacement))) -
    as.integer(stringr::str_count(original,  stringr::fixed(replacement)))
  )

  df[[text_col]]           <- redacted
  df[["n_dict_redacted"]]  <- n_new

  if (verbose) {
    total <- sum(n_new, na.rm = TRUE)
    rows  <- sum(n_new > 0, na.rm = TRUE)
    message("Dictionary: redacted ", total, " name span(s) across ", rows, " row(s).")
  }

  df
}


#' List or inspect the built-in German surname dictionary
#'
#' Returns the surnames in the built-in dictionary as a character vector.
#' Useful for checking coverage or identifying potential false positives
#' before running [deidentify_names_dict()].
#'
#' @param min_length Minimum name length filter (same default as
#'   [deidentify_names_dict()]). Set to `1` to see the full list.
#'
#' @return A character vector of surnames.
#' @export
#' @examples
#' surnames <- lisanon_surnames()
#' length(surnames)
#' head(surnames, 20)
lisanon_surnames <- function(min_length = 4L) {
  dict_path <- system.file("data", "german_surnames.txt", package = "lisanon")
  if (!nchar(dict_path)) {
    rlang::abort("Built-in surname dictionary not found.")
  }
  lines <- readLines(dict_path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[!grepl("^#", lines) & nchar(trimws(lines)) > 0]
  lines[nchar(lines) >= min_length]
}
