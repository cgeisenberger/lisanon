"""
ner_names.py
------------
NER-based name redaction for German clinical free text using spaCy.

Called from R via reticulate.

Requirements:
    pip install "numpy<2" spacy
    python -m spacy download de_core_news_lg
"""

from __future__ import annotations
import re

# Cached spaCy pipeline
_spacy_nlp = None


# ==============================================================================
#  spaCy model loading
# ==============================================================================

def load_spacy_model(model_name="de_core_news_lg"):
    """Load and cache the spaCy model."""
    global _spacy_nlp
    if _spacy_nlp is None:
        try:
            import spacy
            _spacy_nlp = spacy.load(model_name)
        except OSError as exc:
            raise OSError(
                "spaCy model '{}' not found. "
                "Install with: python -m spacy download {}".format(
                    model_name, model_name)
            ) from exc


# ==============================================================================
#  Span utilities
# ==============================================================================

def _merge_spans(spans):
    """Merge overlapping or adjacent character spans."""
    if not spans:
        return []
    sorted_spans = sorted(spans, key=lambda s: s[0])
    merged = [list(sorted_spans[0])]
    for start, end in sorted_spans[1:]:
        if start <= merged[-1][1]:
            merged[-1][1] = max(merged[-1][1], end)
        else:
            merged.append([start, end])
    return [tuple(s) for s in merged]


def _apply_spans(text, spans, replacement):
    """Replace character spans right-to-left to preserve offsets."""
    for start, end in sorted(spans, key=lambda s: s[0], reverse=True):
        text = text[:start] + replacement + text[end:]
    return text


# ==============================================================================
#  Public API -- batch NER redaction (called from R)
# ==============================================================================

def redact_names_batch(texts,
                       replacement="[NAME]",
                       model_name="de_core_news_lg"):
    """Redact PER entities across a list of strings using spaCy.

    Uses nlp.pipe() for efficient batched inference.
    """
    safe_texts = [t if isinstance(t, str) else "" for t in texts]

    load_spacy_model(model_name)

    results = []
    for doc in _spacy_nlp.pipe(safe_texts, batch_size=64):
        spans = [(ent.start_char, ent.end_char)
                 for ent in doc.ents if ent.label_ == "PER"]
        if spans:
            results.append(_apply_spans(doc.text, _merge_spans(spans), replacement))
        else:
            results.append(doc.text)

    return [None if orig is None else r for orig, r in zip(texts, results)]


# ==============================================================================
#  Model loader -- called from R setup_ner()
# ==============================================================================

def load_model(model_name="de_core_news_lg"):
    """Pre-warm the spaCy model. Called by R setup_ner()."""
    load_spacy_model(model_name)


# ==============================================================================
#  Dictionary-based redaction (called from R)
# ==============================================================================

_surname_set = None


def load_surname_dict(dict_path):
    """Load and cache the surname dictionary from a plain-text file."""
    global _surname_set
    if _surname_set is None:
        with open(dict_path, "r", encoding="utf-8") as f:
            names = [
                line.strip() for line in f
                if line.strip() and not line.startswith("#")
            ]
        _surname_set = set(n.lower() for n in names)


def _dict_spans(text, dict_path, title_prefix=True):
    """Return (start, end) char offsets of dictionary surname matches."""
    load_surname_dict(dict_path)
    spans = []
    for m in re.finditer(r'\b([A-Za-z\u00c0-\u024f\u1e00-\u1eff]+)\b', text):
        word = m.group(1)
        if word.lower() in _surname_set:
            start = m.start()
            end   = m.end()
            if title_prefix:
                prefix_m = re.match(
                    r'((?:Dr|Prof|Doz|PD|OA|OA|CA|CA)\.?\s*)$',
                    text[:start]
                )
                if prefix_m:
                    start = start - len(prefix_m.group(1))
            spans.append((start, end))
    return spans


def redact_names_dict_batch(texts,
                            dict_path,
                            replacement="[NAME]",
                            title_prefix=True):
    """Redact dictionary-matched surnames across a list of strings."""
    results = []
    for text in texts:
        if not isinstance(text, str) or not text.strip():
            results.append(text)
            continue
        spans = _dict_spans(text, dict_path, title_prefix=title_prefix)
        if spans:
            results.append(_apply_spans(text, _merge_spans(spans), replacement))
        else:
            results.append(text)
    return [None if orig is None else r for orig, r in zip(texts, results)]
