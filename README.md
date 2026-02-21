# lisanon

**LIS export anonymization pipeline for R**

Pre-processes Laboratory Information System (LIS) Excel/CSV exports before
LLM-assisted annotation. All de-identification runs locally — no patient data
ever leaves your machine unredacted.

## What it does

| Step | Function | What happens |
|------|----------|-------------|
| 1 | `read_lis()` | Reads `.xlsx`, `.xls`, `.csv`, or `.tsv`; applies `janitor::clean_names()` |
| 2 | `anonymize_lis()` | Replaces case IDs with in-memory UUIDs; drops patient, signature, and operational columns |
| 3 | `merge_text_columns()` | Concatenates all free-text/material columns into a single `text` field |
| 4 | `deidentify_names()` | Redacts physician names using spaCy NER + GLiNER (ONNX) |

Run all four steps at once with `lis_pipeline()`.

---

## Installation

### 1. Install the R package

```r
devtools::install("lisanon")
```

### 2. Set up the Python environment (one time per machine)

lisanon uses spaCy for NER via
[reticulate](https://rstudio.github.io/reticulate/). You need to create a
virtual environment with the required packages before first use.

Open a terminal and run:

```bash
# Create the virtual environment
python3 -m venv ~/.venvs/r-spacy

# Activate it
source ~/.venvs/r-spacy/bin/activate    # macOS / Linux

# Install dependencies
# numpy must be pinned to <2 for spaCy/thinc compatibility
pip install "numpy<2" spacy

# Download the German spaCy model (~560 MB)
python -m spacy download de_core_news_lg
```

> **No PyTorch required.** The setup uses only spaCy and its dependencies,
> which work on all platforms including Intel Macs.

> **Windows:** replace the `source` line with:
> `.venvs\r-spacy\Scripts\activate`

### 3. Point reticulate at the virtual environment

Add this line to the **top of every script** that uses lisanon, before any
other reticulate or lisanon call. Or add it to `~/.Rprofile` to make it
permanent:

```r
reticulate::use_virtualenv("~/.venvs/r-spacy", required = TRUE)
```

---

## Quick start

```r
library(lisanon)

# Once per session
reticulate::use_virtualenv("~/.venvs/r-spacy", required = TRUE)
setup_ner()  # loads spaCy; first call downloads spaCy model

# Run the full pipeline using the built-in CoFox preset
result <- lis_pipeline("export_2026-02-20.xlsx", config = "cofox")

# Inspect rows where names were redacted
result |> dplyr::filter(n_names_redacted > 0) |> dplyr::select(auftrag, text)

# Keep the UUID map locally for reverse lookup after annotation
uuid_map <- get_uuid_map(result)
saveRDS(uuid_map, "uuid_map_20260220.rds")  # store securely, locally
```

---

## LIS configuration

### Built-in presets

```r
list_lis_presets()   # shows available presets, currently: "cofox"
load_lis_config("cofox")  # inspect a preset
```

### Custom LIS

Copy the template and adapt it to your column names:

```r
file.copy(lisanon_config_template(), "my_lis.yaml")
# edit my_lis.yaml, then:
result <- lis_pipeline("export.csv", config = "my_lis.yaml")
```

All column names in the config should match **after** `janitor::clean_names()`
is applied. Run `read_lis("your_export.xlsx") |> names()` to see what your
columns look like.

### Individual argument override

Config values are just defaults — individual arguments always win:

```r
# Use cofox preset but lower the GLiNER threshold for higher recall
result <- lis_pipeline("export.xlsx", config = "cofox", gliner_threshold = 0.3)
```

---

## File format support

| Format | Notes |
|--------|-------|
| `.xlsx` | Recommended; all column types read as text |
| `.xls` | Legacy Excel, supported |
| `.csv` | Semicolon-delimited by default (German LIS standard); override with `delim = ","` |
| `.tsv` | Tab-delimited |

For CSV files with German umlauts, try `encoding = "latin1"` if characters
appear garbled.

---

## NER and dictionary redaction

lisanon uses a two-layer approach to name redaction:

| Layer | Function | Method |
|-------|----------|--------|
| 1 | `deidentify_names()` | spaCy `de_core_news_lg` — detects `PER` entities |
| 2 (optional) | `deidentify_names_dict()` | German surname dictionary — whole-word matching |

The dictionary layer is opt-in because common surnames like `Koch`, `Wolf`,
and `Winter` also appear as regular words in pathology text. Use it when
recall is more important than precision, or add `custom_surnames` to target
specific names you know are present.

```r
setup_ner(model_name = "de_core_news_lg")   # default, best accuracy
setup_ner(model_name = "de_core_news_sm")   # faster, less accurate
```

---

## UUID map

Stored as an attribute on the returned tibble — never written to disk by this
package. Save it yourself if you need to reverse-lookup results:

```r
uuid_map <- get_uuid_map(result)
saveRDS(uuid_map, "uuid_map_YYYYMMDD.rds")
```

### Multiple batches

```r
map1   <- get_uuid_map(lis_pipeline("batch1.xlsx", config = "cofox"))
result <- lis_pipeline("batch2.xlsx", config = "cofox", uuid_map = map1)
```

---

## Git / reproducibility

The virtual environment is machine-specific — do not commit it:

```
# .gitignore
.venv/
r-spacy/
__pycache__/
```

A new collaborator clones the repo and follows the **Python environment setup**
steps above to recreate the environment on their machine.

---

## License

MIT
