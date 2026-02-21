#' Anonymize a LIS data frame
#'
#' Replaces case IDs ("Auftrag") with UUIDs using an in-memory mapping table,
#' drops all patient-identifying columns (name, date of birth), the
#' "Unterschriften" (signatures) column, and any additional columns specified
#' via `drop_cols`.
#'
#' The UUID map is stored as an attribute on the returned tibble so you can
#' retrieve it with `attr(result, "uuid_map")` if you need to reverse-look up
#' results after LLM annotation.
#'
#' @param df A tibble as returned by [read_lis()].
#' @param auftrag_prefix Prefix used to identify the case ID column.
#'   Defaults to `"auftrag"`.
#' @param patient_prefixes Character vector of prefixes identifying
#'   patient-identifying columns to remove (name, DOB).
#' @param unterschriften_prefix Prefix identifying the signatures column.
#' @param drop_cols Character vector of additional column name prefixes to drop
#'   before LLM submission. Defaults to `c("eingangsdatum", "freigabedatum",
#'   "freigabe_druckdatum", "einsender")` — operational fields that are not
#'   needed for annotation and may contain identifiable information.
#'   Set to `character(0)` to disable.
#' @param uuid_map Optional existing UUID map (named character vector,
#'   names = original IDs, values = UUIDs) to extend. Useful when processing
#'   multiple batches and wanting consistent mappings.
#'
#' @return A tibble with:
#'   * Case IDs replaced by UUIDs
#'   * Patient-identifying, signature, and operational columns removed
#'   * Attribute `"uuid_map"` containing the full mapping for this session
#'
#' @export
#' @examples
#' \dontrun{
#' raw   <- read_lis("export.xlsx")
#' anon  <- anonymize_lis(raw)
#' map   <- attr(anon, "uuid_map")  # keep this safe locally!
#'
#' # Keep eingangsdatum for downstream use
#' anon  <- anonymize_lis(raw, drop_cols = c("freigabedatum", "einsender"))
#' }
anonymize_lis <- function(df,
                          auftrag_prefix        = "auftrag",
                          patient_prefixes      = c("patient_vor", "patient_nach", "patient_geb"),
                          unterschriften_prefix = "unterschrift",
                          drop_cols             = c("eingangsdatum", "freigabedatum",
                                                    "freigabe_druckdatum", "einsender"),
                          uuid_map              = NULL) {

  # ── 1. Locate the case ID column ──────────────────────────────────────────
  auftrag_cols <- find_cols(df, auftrag_prefix)
  if (length(auftrag_cols) == 0) {
    rlang::abort(paste0("No column matching prefix '", auftrag_prefix, "' found."))
  }
  if (length(auftrag_cols) > 1) {
    rlang::warn(paste0("Multiple columns match '", auftrag_prefix, "': ",
                       paste(auftrag_cols, collapse = ", "),
                       ". Using the first one."))
    auftrag_cols <- auftrag_cols[1]
  }
  auftrag_col <- auftrag_cols[1]

  # ── 2. Build / extend UUID map ────────────────────────────────────────────
  original_ids <- unique(df[[auftrag_col]])
  original_ids <- original_ids[!is.na(original_ids)]

  if (is.null(uuid_map)) uuid_map <- character(0)

  new_ids <- setdiff(original_ids, names(uuid_map))
  if (length(new_ids) > 0) {
    new_uuids        <- vapply(new_ids, function(.) uuid::UUIDgenerate(), character(1))
    names(new_uuids) <- new_ids
    uuid_map         <- c(uuid_map, new_uuids)
  }

  message("UUID map contains ", length(uuid_map), " entries (",
          length(new_ids), " newly generated).")

  # ── 3. Replace case IDs with UUIDs ───────────────────────────────────────
  df[[auftrag_col]] <- dplyr::recode(df[[auftrag_col]], !!!uuid_map)

  # ── 4. Drop patient-identifying columns ──────────────────────────────────
  patient_cols <- unlist(lapply(patient_prefixes, function(pfx) find_cols(df, pfx)))
  if (length(patient_cols) > 0) {
    message("Dropping patient columns: ", paste(patient_cols, collapse = ", "))
    df <- dplyr::select(df, -dplyr::all_of(patient_cols))
  } else {
    rlang::warn("No patient-identifying columns found to drop.")
  }

  # ── 5. Drop Unterschriften column ─────────────────────────────────────────
  sig_cols <- find_cols(df, unterschriften_prefix)
  if (length(sig_cols) > 0) {
    message("Dropping signature columns: ", paste(sig_cols, collapse = ", "))
    df <- dplyr::select(df, -dplyr::all_of(sig_cols))
  } else {
    rlang::warn("No signature column found to drop.")
  }

  # ── 6. Drop additional operational columns ────────────────────────────────
  if (length(drop_cols) > 0) {
    extra_cols <- unlist(lapply(drop_cols, function(pfx) find_cols(df, pfx)))
    if (length(extra_cols) > 0) {
      message("Dropping additional columns: ", paste(extra_cols, collapse = ", "))
      df <- dplyr::select(df, -dplyr::all_of(extra_cols))
    }
  }

  # ── 7. Attach the map as an attribute ─────────────────────────────────────
  attr(df, "uuid_map") <- uuid_map

  df
}


#' Retrieve the UUID map from an anonymized data frame
#'
#' Convenience wrapper around `attr(df, "uuid_map")` that also validates the
#' attribute exists.
#'
#' @param df A tibble as returned by [anonymize_lis()].
#' @return A named character vector (names = original IDs, values = UUIDs).
#' @export
get_uuid_map <- function(df) {
  m <- attr(df, "uuid_map")
  if (is.null(m)) {
    rlang::abort("No UUID map found. Was this data frame produced by anonymize_lis()?")
  }
  m
}
