#' Read a LIS export file
#'
#' Reads an Excel (`.xlsx`, `.xls`) or delimited text (`.csv`, `.tsv`) export
#' from a Laboratory Information System. Column names are cleaned with
#' [janitor::clean_names()] by default to produce consistent snake_case, which
#' is required for the default prefix matching in downstream functions.
#'
#' @param path Path to the file. Supported formats: `.xlsx`, `.xls`, `.csv`,
#'   `.tsv`.
#' @param sheet Sheet name or index (Excel only). Defaults to the first sheet.
#' @param skip Number of rows to skip before the header. Defaults to 0.
#' @param clean_names If `TRUE` (default), applies [janitor::clean_names()] to
#'   produce snake_case column names.
#' @param delim Delimiter for CSV/TSV files. If `NULL` (default), auto-detected:
#'   `;` for `.csv` (common in German LIS exports), `\t` for `.tsv`.
#' @param encoding Encoding for CSV/TSV files. Defaults to `"UTF-8"`. Try
#'   `"latin1"` or `"windows-1252"` if German umlauts appear garbled.
#' @param expected_prefixes Character vector of column name prefixes used for
#'   validation warnings. If `NULL`, uses the CoFox defaults. Pass
#'   `character(0)` to suppress validation entirely.
#'
#' @return A tibble with the raw LIS data and clean snake_case column names.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- read_lis("export_2026-02-20.xlsx")
#' df <- read_lis("export.csv", encoding = "latin1")
#' df <- read_lis("export.csv", delim = ",")  # override auto-detection
#' }
read_lis <- function(path,
                     sheet             = 1,
                     skip              = 0,
                     clean_names       = TRUE,
                     delim             = NULL,
                     encoding          = "UTF-8",
                     expected_prefixes = c("auftrag", "eingang",
                                           "patient_vor", "patient_nach",
                                           "patient_geb", "unterschrift")) {

  if (!file.exists(path)) {
    rlang::abort(paste0("File not found: ", path))
  }

  ext <- tolower(tools::file_ext(path))

  # ── Read based on file type ───────────────────────────────────────────────
  df <- switch(ext,

    xlsx = {
      readxl::read_excel(path, sheet = sheet, skip = skip, col_types = "text")
    },

    xls = {
      readxl::read_excel(path, sheet = sheet, skip = skip, col_types = "text")
    },

    csv = {
      sep <- delim %||% ";"  # German LIS exports typically use semicolons
      utils::read.csv(path, sep = sep, fileEncoding = encoding,
                      colClasses = "character", skip = skip,
                      check.names = FALSE, stringsAsFactors = FALSE) |>
        tibble::as_tibble()
    },

    tsv = {
      sep <- delim %||% "\t"
      utils::read.csv(path, sep = sep, fileEncoding = encoding,
                      colClasses = "character", skip = skip,
                      check.names = FALSE, stringsAsFactors = FALSE) |>
        tibble::as_tibble()
    },

    rlang::abort(paste0(
      "Unsupported file type: '.", ext, "'. ",
      "Supported formats: .xlsx, .xls, .csv, .tsv"
    ))
  )

  # ── Clean column names ────────────────────────────────────────────────────
  if (clean_names) {
    df <- janitor::clean_names(df)
  }

  # ── Validate expected columns ─────────────────────────────────────────────
  if (length(expected_prefixes) > 0) {
    found <- vapply(expected_prefixes, function(pfx) {
      any(stringr::str_starts(names(df), pfx))
    }, logical(1))

    missing <- expected_prefixes[!found]
    if (length(missing) > 0) {
      rlang::warn(paste0(
        "Could not find columns matching these expected prefixes: ",
        paste(missing, collapse = ", "),
        "\nActual columns: ", paste(names(df), collapse = ", ")
      ))
    }
  }

  message("Read ", nrow(df), " rows and ", ncol(df), " columns from ",
          basename(path), " [", toupper(ext), "]")
  tibble::as_tibble(df)
}


#' Find column names by prefix
#'
#' Helper to resolve truncated column names robustly. Returns all column names
#' that start with the given prefix (case-insensitive).
#'
#' @param df A data frame.
#' @param prefix Character string prefix to match.
#'
#' @return A character vector of matching column names.
#' @keywords internal
find_cols <- function(df, prefix) {
  names(df)[stringr::str_starts(names(df), stringr::regex(prefix, ignore_case = TRUE))]
}
