#' Merge free-text columns into a single material/findings field
#'
#' After the "Unterschriften" column has been dropped by [anonymize_lis()], all
#' remaining columns to the right of the last known structured column are
#' treated as free-text material/findings columns. These are concatenated into
#' a single `text` column, with empty/NA values silently ignored.
#'
#' In practice the split between "structured" and "free-text" columns is
#' determined by the `structured_prefixes` argument: every column whose name
#' does *not* start with one of those prefixes AND whose position is to the
#' right of the last structured column is merged.
#'
#' @param df A tibble as returned by [anonymize_lis()].
#' @param structured_prefixes Character vector of column-name prefixes that
#'   belong to the structured (left-hand) part of the export. Defaults cover
#'   the standard CoFox field names.
#' @param text_col Name of the new merged column. Defaults to `"text"`.
#' @param sep Separator inserted between non-empty fragments. Defaults to
#'   `" "` (single space).
#'
#' @return A tibble with all free-text columns replaced by a single `text`
#'   column appended at the right.
#'
#' @export
#' @examples
#' \dontrun{
#' anon   <- anonymize_lis(read_lis("export.xlsx"))
#' merged <- merge_text_columns(anon)
#' }
merge_text_columns <- function(df,
                               structured_prefixes = c(
                                 "auftrag", "eingang", "dokument",
                                 "freigabe", "einsender", "abteilung"
                               ),
                               text_col = "text",
                               sep      = " ") {

  col_names <- names(df)

  # A column is "structured" if its name starts with any of the known prefixes
  is_structured <- vapply(col_names, function(cn) {
    any(stringr::str_starts(cn, stringr::regex(structured_prefixes,
                                               ignore_case = TRUE)))
  }, logical(1))

  # Find the index of the last structured column
  last_structured_idx <- max(which(is_structured), na.rm = TRUE)

  if (is.infinite(last_structured_idx)) {
    rlang::abort("Could not identify any structured columns. Check `structured_prefixes`.")
  }

  structured_cols  <- col_names[seq_len(last_structured_idx)]
  freetext_cols    <- col_names[seq(last_structured_idx + 1, length(col_names))]

  if (length(freetext_cols) == 0) {
    rlang::warn("No free-text columns detected after the structured block. ",
                "Returning df unchanged.")
    return(df)
  }

  message("Merging ", length(freetext_cols), " free-text column(s): ",
          paste(freetext_cols, collapse = ", "))

  # Concatenate non-empty values row-wise
  df[[text_col]] <- apply(df[, freetext_cols, drop = FALSE], 1, function(row) {
    fragments <- row[!is.na(row) & nchar(trimws(row)) > 0]
    if (length(fragments) == 0) return(NA_character_)
    paste(trimws(fragments), collapse = sep)
  })

  # Drop the individual free-text columns
  df <- dplyr::select(df, dplyr::all_of(structured_cols), dplyr::all_of(text_col))

  df
}
