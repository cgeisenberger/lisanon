#' Set up the spaCy NER environment
#'
#' Loads the Python helper module and pre-warms the spaCy model.
#' Call this once at the start of your session before [deidentify_names()].
#'
#' @param model_name spaCy model to use. Defaults to `"de_core_news_lg"`.
#'   Use `"de_core_news_sm"` for a faster, smaller alternative.
#' @param python Path to Python executable. If `NULL`, reticulate uses its
#'   currently configured Python.
#' @param prewarm If `TRUE` (default), loads the model immediately so the
#'   first call to [deidentify_names()] is not slow.
#'
#' @return Invisibly returns the Python session.
#' @export
#' @examples
#' \dontrun{
#' setup_ner()
#' setup_ner(model_name = "de_core_news_sm")  # faster, less accurate
#' }
setup_ner <- function(model_name = "de_core_news_lg",
                      python     = NULL,
                      prewarm    = TRUE) {

  if (!is.null(python)) reticulate::use_python(python, required = TRUE)

  py_script <- system.file("python", "ner_names.py", package = "lisanon")
  if (!nchar(py_script)) {
    py_script <- file.path(find.package("lisanon", quiet = TRUE),
                           "inst", "python", "ner_names.py")
  }
  if (!file.exists(py_script)) {
    rlang::abort("Cannot find inst/python/ner_names.py. Is the package installed correctly?")
  }

  reticulate::source_python(py_script)

  if (prewarm) {
    message("Loading spaCy model '", model_name, "' (this may take a moment)...")
    tryCatch(
      load_model(model_name = model_name),
      error = function(e) rlang::abort(conditionMessage(e))
    )
    message("spaCy model ready.")
  }

  invisible(reticulate::py)
}


#' Remove person names from free text using spaCy NER
#'
#' Applies the spaCy German NER model to detect and redact `PER` entities
#' from the merged text column.
#'
#' Call [setup_ner()] once before using this function. For additional
#' coverage, follow up with [deidentify_names_dict()].
#'
#' @param df A tibble as returned by [merge_text_columns()].
#' @param text_col Name of the column containing merged free text.
#'   Defaults to `"text"`.
#' @param replacement Placeholder substituted for each detected name span.
#'   Defaults to `"[NAME]"`.
#' @param model_name spaCy model name. Defaults to `"de_core_news_lg"`.
#' @param verbose Print a redaction summary. Default `TRUE`.
#'
#' @return The input tibble with names redacted in `text_col`, plus a new
#'   integer column `n_names_redacted` with the count per row.
#'
#' @export
#' @examples
#' \dontrun{
#' setup_ner()
#' clean <- deidentify_names(merged_df)
#' clean <- clean |> deidentify_names_dict()  # optional dictionary step
#' }
deidentify_names <- function(df,
                             text_col    = "text",
                             replacement = "[NAME]",
                             model_name  = "de_core_news_lg",
                             verbose     = TRUE) {

  if (!text_col %in% names(df)) {
    rlang::abort(paste0("Column '", text_col, "' not found. Run merge_text_columns() first."))
  }

  if (!reticulate::py_has_attr(reticulate::py, "redact_names_batch")) {
    rlang::abort("redact_names_batch() not found. Did you call setup_ner()?")
  }

  original <- df[[text_col]]

  message("Running spaCy NER on ", length(original), " texts...")

  redacted <- reticulate::py$redact_names_batch(
    as.list(original),
    replacement = replacement,
    model_name  = model_name
  )
  redacted <- unlist(redacted)

  n_redacted <- stringr::str_count(redacted, stringr::fixed(replacement))
  n_original <- stringr::str_count(original, stringr::fixed(replacement))
  n_redacted <- pmax(0L, as.integer(n_redacted - n_original))

  df[[text_col]]           <- redacted
  df[["n_names_redacted"]] <- n_redacted

  if (verbose) {
    total <- sum(n_redacted, na.rm = TRUE)
    rows  <- sum(n_redacted > 0, na.rm = TRUE)
    message("Redacted ", total, " name span(s) across ", rows, " row(s).")
  }

  df
}
