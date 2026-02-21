# ============================================================
#  lisanon — workflow test script
# ============================================================

# ── 0. Setup ----------------------------------------------------------
#devtools::install("~/Dropbox/Research/Packages/lisanon", quiet = TRUE)


# Restart session after installing, then continue from here

library(lisanon)

# Python environment — set up once in terminal if not already done:
#   python3 -m venv ~/.venvs/r-spacy
#   source ~/.venvs/r-spacy/bin/activate
#   pip install "numpy<2" spacy onnxruntime gliner
#   python -m spacy download de_core_news_lg

reticulate::use_virtualenv("~/.venvs/r-spacy", required = TRUE)


# ── 1. Synthetic test data --------------------------------------------
synthetic_data <- tibble::tibble(
  auftrag              = c("E/2026/004890", "E/2026/004877", "E/2026/004360"),
  eingangsdatum        = c("19.02.2026",    "19.02.2026",    "16.02.2026"),
  dokument             = c("Befund",        "Befund",        "Befund"),
  freigabedatum        = c(NA,              NA,              "18.02.2026"),
  freigabe_druckdatum  = c(NA,              NA,              NA),
  patient_nachname     = c("Musterfrau",    "Mustermann",    "Beispiel"),
  patient_vorname      = c("Maria",         "Hans",          "Erika"),
  patient_geb_datum    = c("01.01.1960",    "15.06.1955",    "22.03.1970"),
  einsender            = c("Kinderchirurgie", "Abteilung f.", "Urologisch gei"),
  unterschriften       = c("Dr. Schmidt",   "Dr. Meier",     "Dr. Wagner"),
  material             = c(
    "1. Meckel-Divertikel. 2. Thrombus Leberarterie links.",
    "Lunge rechts. Z. n. Frischgewebsentnahme am 19.02.2026.",
    "1. Nierentumor links. 2. Nachresekat Nierentumor links."
  ),
  material_2           = c(
    NA,
    "Nebenniere links (bei Bergen im Beutel rupturiert).",
    "Frischgewebsentnahme am 16.02.2026 um 12 Uhr. Dr. Müller anwesend."
  )
)

# Write to temporary xlsx and csv for format tests
tmp_xlsx <- tempfile(fileext = ".xlsx")
tmp_csv  <- tempfile(fileext = ".csv")
writexl::write_xlsx(synthetic_data, tmp_xlsx)
write.csv2(synthetic_data, tmp_csv, row.names = FALSE)  # write.csv2 uses ; delimiter
cat("Temporary files written:\n  xlsx:", tmp_xlsx, "\n  csv: ", tmp_csv, "\n")


# ── 2. read_lis() -----------------------------------------------------
cat("\n══════════════════════════════════════════\n")
cat(" STEP 1 — read_lis()\n")
cat("══════════════════════════════════════════\n")

raw_xlsx <- read_lis(tmp_xlsx)
raw_csv  <- read_lis(tmp_csv)

stopifnot(all(names(raw_xlsx) == names(raw_csv)))
cat("\n✓ xlsx and csv produce identical column names\n")
cat("  Columns:", paste(names(raw_xlsx), collapse = ", "), "\n")


# ── 3. list_lis_presets() and load_lis_config() -----------------------
cat("\n══════════════════════════════════════════\n")
cat(" Config system\n")
cat("══════════════════════════════════════════\n")

cat("Available presets:", paste(list_lis_presets(), collapse = ", "), "\n")

cfg <- load_lis_config("cofox")
cat("CoFox drop_cols:", paste(cfg$drop_cols, collapse = ", "), "\n")
cat("CoFox structured_prefixes:", paste(cfg$structured_prefixes, collapse = ", "), "\n")

stopifnot("auftrag_prefix" %in% names(cfg))
stopifnot(length(cfg$patient_prefixes) > 0)
cat("\n✓ CoFox config loaded and validated\n")


# ── 4. anonymize_lis() ------------------------------------------------
cat("\n══════════════════════════════════════════\n")
cat(" STEP 2 — anonymize_lis()\n")
cat("══════════════════════════════════════════\n")

anon <- anonymize_lis(raw_xlsx)

cat("\nUUID map:\n")
print(get_uuid_map(anon))
cat("\nRemaining columns:", paste(names(anon), collapse = ", "), "\n")

# Patient columns removed
stopifnot(!any(c("patient_vorname", "patient_nachname", "patient_geb_datum") %in% names(anon)))
cat("\n✓ Patient columns removed\n")

# Signature column removed
stopifnot(!"unterschriften" %in% names(anon))
cat("✓ Signature column removed\n")

# Operational columns removed
stopifnot(!any(c("eingangsdatum", "freigabedatum", "einsender") %in% names(anon)))
cat("✓ Operational columns removed\n")

# UUIDs in place (UUIDs are always 36 characters)
stopifnot(all(nchar(anon$auftrag) == 36))
cat("✓ Case IDs replaced with UUIDs\n")


# ── 5. merge_text_columns() -------------------------------------------
cat("\n══════════════════════════════════════════\n")
cat(" STEP 3 — merge_text_columns()\n")
cat("══════════════════════════════════════════\n")

merged <- merge_text_columns(anon)

cat("\nMerged texts:\n")
for (i in seq_len(nrow(merged))) {
  cat(sprintf("  Row %d: %s\n", i, merged$text[i]))
}

stopifnot("text" %in% names(merged))
stopifnot(!any(c("material", "material_2") %in% names(merged)))
cat("\n✓ Free-text columns merged into 'text'\n")
cat("✓ Original free-text columns removed\n")


# ── 6. setup_ner() and deidentify_names() ----------------------------
cat("\n══════════════════════════════════════════\n")
cat(" STEP 4 — NER de-identification\n")
cat("══════════════════════════════════════════\n")

setup_ner()  # loads spaCy + GLiNER — slow on first call, cached after

clean <- deidentify_names(merged)

cat("\nCleaned texts:\n")
for (i in seq_len(nrow(clean))) {
  cat(sprintf("  Row %d [%d name(s) redacted]: %s\n",
              i, clean$n_names_redacted[i], clean$text[i]))
}

stopifnot("n_names_redacted" %in% names(clean))

if (clean$n_names_redacted[3] > 0) {
  cat("\n✓ Physician name (Dr. Müller) redacted in row 3\n")
} else {
  cat("\n⚠ NER did not catch Dr. Müller in row 3 — check model / threshold\n")
}


# ── 6. Dictionary de-identification (optional step) -------------------
cat("\n══════════════════════════════════════════\n")
cat(" STEP 5 — Dictionary name redaction\n")
cat("══════════════════════════════════════════\n")

# Inspect built-in dictionary
surnames <- lisanon_surnames()
cat("Built-in dictionary:", length(surnames), "surnames (min length 4)\n")
stopifnot(length(surnames) > 100)
stopifnot("Müller" %in% surnames)

# Apply dictionary redaction on top of NER output
dict_clean <- deidentify_names_dict(clean)

cat("\nTexts after dictionary redaction:\n")
for (i in seq_len(nrow(dict_clean))) {
  cat(sprintf("  Row %d [NER: %d, dict: %d]: %s\n",
              i,
              dict_clean$n_names_redacted[i],
              dict_clean$n_dict_redacted[i],
              dict_clean$text[i]))
}

stopifnot("n_dict_redacted" %in% names(dict_clean))
cat("\n✓ Dictionary redaction completed\n")

# Custom surnames
dict_custom <- deidentify_names_dict(clean,
                                     custom_surnames = c("Mustermann", "Musterfrau"))
stopifnot("n_dict_redacted" %in% names(dict_custom))
cat("✓ Custom surnames accepted\n")


# ── 7. Full lis_pipeline() with config --------------------------------
cat("\n══════════════════════════════════════════\n")
cat(" Full lis_pipeline() with cofox preset\n")
cat("══════════════════════════════════════════\n")

result <- lis_pipeline(tmp_xlsx, config = "cofox", use_dict = TRUE)

cat("\nFinal columns:", paste(names(result), collapse = ", "), "\n")
cat("Rows:", nrow(result), "\n")
cat("UUID map entries:", length(get_uuid_map(result)), "\n")

stopifnot(nrow(result) == 3)
stopifnot("text" %in% names(result))
stopifnot("n_names_redacted" %in% names(result))
stopifnot("n_dict_redacted" %in% names(result))
stopifnot(length(get_uuid_map(result)) == 3)

cat("\n✓ lis_pipeline() completed successfully\n")


# ── 8. Same pipeline from CSV ----------------------------------------
cat("\n══════════════════════════════════════════\n")
cat(" Full lis_pipeline() from CSV\n")
cat("══════════════════════════════════════════\n")

result_csv <- lis_pipeline(tmp_csv, config = "cofox", use_dict = TRUE)

stopifnot(nrow(result_csv) == nrow(result))
stopifnot(all(names(result_csv) == names(result)))
cat("\n✓ CSV pipeline produces same structure as xlsx\n")


cat("\n══════════════════════════════════════════\n")
cat(" All tests passed.\n")
cat("══════════════════════════════════════════\n")
