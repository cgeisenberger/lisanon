#' Run the full LIS anonymization pipeline
#'
#' Convenience wrapper that calls [read_lis()], [anonymize_lis()],
#' [merge_text_columns()], [deidentify_names()], and optionally
#' [deidentify_names_dict()] in sequence.
#'
#' You must call [setup_ner()] once per session before using this function.
#'
#' @param path Path to the LIS export file (`.xlsx`, `.xls`, `.csv`, `.tsv`).
#' @param config Optional LIS configuration: preset name (e.g. `"cofox"`) or
#'   path to a custom `.yaml` file. See [load_lis_config()].
#' @param sheet Sheet to read (Excel only).
#' @param skip Rows to skip before the header.
#' @param delim Delimiter for CSV/TSV (auto-detected if `NULL`).
#' @param encoding Encoding for CSV/TSV. Defaults to `"UTF-8"`.
#' @param auftrag_prefix Prefix for the case ID column.
#' @param patient_prefixes Prefixes of patient-identifying columns to drop.
#' @param drop_cols Additional column prefixes to drop before LLM submission.
#' @param structured_prefixes Structured column prefixes kept as-is;
#'   everything else is merged into `text_col`.
#' @param text_col Name of the merged text output column.
#' @param replacement Redaction placeholder. Defaults to `"[NAME]"`.
#' @param model_name spaCy model name. Defaults to `"de_core_news_lg"`.
#' @param use_dict If `TRUE`, runs [deidentify_names_dict()] as an additional
#'   step after NER. Catches surnames missed by spaCy using a German surname
#'   dictionary. Default `FALSE`. See [deidentify_names_dict()] for notes
#'   on false positives.
#' @param dict_min_length Minimum surname length for dictionary matching.
#'   Defaults to `4`. Only used when `use_dict = TRUE`.
#' @param custom_surnames Optional character vector of additional surnames.
#'   Only used when `use_dict = TRUE`.
#' @param uuid_map Optional existing UUID map to extend (named character vector).
#' @param verbose Print progress messages. Default `TRUE`.
#'
#' @return A tibble with case IDs replaced by UUIDs, PII columns removed,
#'   free text merged, names redacted, and attribute `"uuid_map"` attached.
#'
#' @export
#' @examples
#' \dontrun{
#' setup_ner()
#'
#' # Basic
#' result <- lis_pipeline("export.xlsx", config = "cofox")
#'
#' # With dictionary fallback
#' result <- lis_pipeline("export.xlsx", config = "cofox", use_dict = TRUE)
#'
#' # Custom surnames + CSV input
#' result <- lis_pipeline("export.csv", config = "cofox",
#'                        use_dict = TRUE,
#'                        custom_surnames = c("Geisenberger", "Huber"))
#' }
lis_pipeline <- function(path,
                         config              = NULL,
                         sheet               = 1,
                         skip                = 0,
                         delim               = NULL,
                         encoding            = "UTF-8",
                         auftrag_prefix      = NULL,
                         patient_prefixes    = NULL,
                         drop_cols           = NULL,
                         structured_prefixes = NULL,
                         text_col            = "text",
                         replacement         = "[NAME]",
                         model_name          = "de_core_news_lg",
                         use_dict            = FALSE,
                         dict_min_length     = 4L,
                         custom_surnames     = NULL,
                         redact_case_ids     = TRUE,
                         case_id_pattern     = NULL,
                         uuid_map            = NULL,
                         verbose             = TRUE) {

  # ── Resolve config ─────────────────────────────────────────────────────────
  cfg <- if (!is.null(config)) load_lis_config(config) else list()

  auftrag_prefix      <- auftrag_prefix      %||% cfg$auftrag_prefix      %||% "auftrag"
  patient_prefixes    <- patient_prefixes    %||% cfg$patient_prefixes    %||%
                           c("patient_vor", "patient_nach", "patient_geb")
  drop_cols           <- drop_cols           %||% cfg$drop_cols           %||%
                           c("eingangsdatum", "freigabedatum",
                             "freigabe_druckdatum", "einsender")
  structured_prefixes <- structured_prefixes %||% cfg$structured_prefixes %||%
                           c("auftrag", "dokument")
  expected_prefixes   <- cfg$expected_prefixes %||%
                           c("auftrag", "eingang", "patient_vor",
                             "patient_nach", "patient_geb", "unterschrift")

  # ── Step 1: Read ───────────────────────────────────────────────────────────
  if (verbose) message("\n── Step 1/5: Reading export ─────────────────────────")
  df <- read_lis(path, sheet = sheet, skip = skip,
                 delim = delim, encoding = encoding,
                 expected_prefixes = expected_prefixes)

  # ── Step 2: Anonymize ──────────────────────────────────────────────────────
  if (verbose) message("\n── Step 2/5: Anonymizing case IDs & removing PII ───")
  df <- anonymize_lis(df,
                      auftrag_prefix   = auftrag_prefix,
                      patient_prefixes = patient_prefixes,
                      drop_cols        = drop_cols,
                      uuid_map         = uuid_map)
  .uuid_map <- get_uuid_map(df)

  # ── Step 3: Merge free-text columns ───────────────────────────────────────
  if (verbose) message("\n── Step 3/5: Merging free-text columns ──────────────")
  df <- merge_text_columns(df,
                           structured_prefixes = structured_prefixes,
                           text_col            = text_col)

  # ── Step 4: spaCy NER redaction ───────────────────────────────────────────
  if (verbose) message("\n── Step 4/5: spaCy NER name redaction ───────────────")
  df <- deidentify_names(df,
                         text_col    = text_col,
                         replacement = replacement,
                         model_name  = model_name,
                         verbose     = verbose)

  attr(df, "uuid_map") <- .uuid_map

  # ── Step 5: Case ID redaction ─────────────────────────────────────────────
  if (redact_case_ids) {
    if (verbose) message("\n── Step 5/5: Case ID redaction ──────────────────────")
    df <- deidentify_case_ids(df,
                              text_col    = text_col,
                              pattern     = case_id_pattern,
                              verbose     = verbose)
    attr(df, "uuid_map") <- .uuid_map
  }

  # ── Step 6 (optional): Dictionary redaction ───────────────────────────────
  if (use_dict) {
    if (verbose) message("\n── Step 6/6: Dictionary name redaction ──────────────")
    df <- deidentify_names_dict(df,
                                text_col        = text_col,
                                replacement     = replacement,
                                min_length      = dict_min_length,
                                custom_surnames = custom_surnames,
                                verbose         = verbose)
    attr(df, "uuid_map") <- .uuid_map
  }

  if (verbose) {
    message("\n── Done ──────────────────────────────────────────────")
    message(nrow(df), " rows ready for LLM annotation.")
    message("Retrieve UUID map with: get_uuid_map(result)")
  }

  df
}
