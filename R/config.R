#' List available built-in LIS presets
#'
#' Returns the names of LIS configuration presets shipped with the package.
#' Pass any of these names as the `config` argument to [lis_pipeline()].
#'
#' @return A character vector of preset names (without `.yaml` extension).
#' @export
#' @examples
#' list_lis_presets()
list_lis_presets <- function() {
  config_dir <- system.file("config", package = "lisanon")
  yamls <- list.files(config_dir, pattern = "\\.yaml$", full.names = FALSE)
  # Exclude the template
  presets <- yamls[yamls != "template.yaml"]
  tools::file_path_sans_ext(presets)
}


#' Load a LIS configuration
#'
#' Loads a YAML configuration file and returns it as a named list. Accepts
#' either a built-in preset name (e.g. `"cofox"`) or a path to a custom
#' `.yaml` file.
#'
#' Use [list_lis_presets()] to see available built-in presets, and
#' `lisanon_config_template()` to get the path to the template for creating
#' your own.
#'
#' @param config Either:
#'   * A preset name string (e.g. `"cofox"`) for built-in configs, or
#'   * A file path to a custom `.yaml` config file.
#'
#' @return A named list with keys: `lis`, `description`, `auftrag_prefix`,
#'   `patient_prefixes`, `unterschriften_prefix`, `drop_cols`,
#'   `structured_prefixes`, `expected_prefixes`.
#'
#' @export
#' @examples
#' cfg <- load_lis_config("cofox")
#' cfg$drop_cols
load_lis_config <- function(config) {

  # Resolve path: preset name or direct file path
  if (!grepl("[/\\\\]", config) && !grepl("\\.yaml$", config)) {
    # Treat as a preset name
    preset_path <- system.file("config", paste0(config, ".yaml"),
                               package = "lisanon")
    if (!nchar(preset_path)) {
      available <- paste(list_lis_presets(), collapse = ", ")
      rlang::abort(paste0(
        "Preset '", config, "' not found. ",
        "Available presets: ", available, "\n",
        "Or pass a path to a custom .yaml file."
      ))
    }
    path <- preset_path
  } else {
    path <- config
    if (!file.exists(path)) {
      rlang::abort(paste0("Config file not found: ", path))
    }
  }

  cfg <- yaml::read_yaml(path)

  # Validate required keys
  required_keys <- c("auftrag_prefix", "patient_prefixes",
                     "unterschriften_prefix", "structured_prefixes")
  missing_keys <- setdiff(required_keys, names(cfg))
  if (length(missing_keys) > 0) {
    rlang::abort(paste0(
      "Config file is missing required keys: ",
      paste(missing_keys, collapse = ", "), "\n",
      "See the template at: ", lisanon_config_template()
    ))
  }

  # Ensure list fields are character vectors
  cfg$patient_prefixes    <- as.character(cfg$patient_prefixes)
  cfg$drop_cols           <- as.character(cfg$drop_cols %||% character(0))
  cfg$structured_prefixes <- as.character(cfg$structured_prefixes)
  cfg$expected_prefixes   <- as.character(cfg$expected_prefixes %||% character(0))

  message("Loaded LIS config: ", cfg$lis %||% basename(path),
          " — ", cfg$description %||% "")
  cfg
}


#' Get the path to the config template file
#'
#' Returns the path to the bundled `template.yaml` which you can copy and
#' adapt for your own LIS system.
#'
#' @return Path to `template.yaml` as a character string.
#' @export
#' @examples
#' \dontrun{
#' # Copy template to your project directory
#' file.copy(lisanon_config_template(), "my_lis.yaml")
#' }
lisanon_config_template <- function() {
  system.file("config", "template.yaml", package = "lisanon")
}


# Internal null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
