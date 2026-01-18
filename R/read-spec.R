#' Read a specification file
#'
#' Reads and parses a YAML or JSON specification file into a niche_spec object.
#' Minimal metadata is attached but deep validation is not performed at this stage.
#'
#' @param path Character scalar path to a YAML (.yml, .yaml) or JSON (.json) file
#'
#' @return A niche_spec object with parsed specification and minimal metadata
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- read_spec("my_study.yml")
#' }
read_spec <- function(path) {
  # Validate file exists using nicheCore assertion
  if (!file.exists(path)) {
    nicheCore::niche_abort(
      "File does not exist",
      details = list(path = path),
      help = "Provide a valid file path to a YAML or JSON specification file"
    )
  }

  # Normalize path
  path_normalized <- tryCatch(
    normalize_path(path),
    error = function(e) {
      nicheCore::niche_abort(
        "Failed to normalize path",
        details = list(path = path, error = conditionMessage(e)),
        help = "Ensure the file path is valid and accessible"
      )
    }
  )

  # Determine file type by extension
  ext <- tolower(tools::file_ext(path_normalized))

  # Check extension before parsing
  if (!ext %in% c("yml", "yaml", "json")) {
    nicheCore::niche_abort(
      "Unsupported file extension",
      details = list(path = path_normalized, extension = ext),
      help = "Use .yml, .yaml, or .json file extensions"
    )
  }

  # Parse based on extension
  parsed <- tryCatch(
    {
      if (ext %in% c("yml", "yaml")) {
        yaml::read_yaml(path_normalized)
      } else if (ext == "json") {
        jsonlite::fromJSON(path_normalized, simplifyVector = FALSE)
      }
    },
    error = function(e) {
      nicheCore::niche_abort(
        "Failed to parse specification file",
        details = list(
          path = path_normalized,
          extension = ext,
          error = conditionMessage(e)
        ),
        help = "Ensure the file contains valid YAML or JSON"
      )
    }
  )

  # Attach minimal metadata
  # Set schema_version if missing
  if (is.null(parsed$schema_version)) {
    parsed$schema_version <- schema_version_current()
  }

  # Attach source path
  parsed$source <- path_normalized

  # Normalize YAML list structures to R vectors
  # YAML parsers create named lists for mappings, but we need named character vectors
  if (!is.null(parsed$vars$rename) && is.list(parsed$vars$rename)) {
    if (length(parsed$vars$rename) > 0) {
      parsed$vars$rename <- unlist(parsed$vars$rename, use.names = TRUE)
    } else {
      parsed$vars$rename <- stats::setNames(character(0), character(0))
    }
  }

  if (!is.null(parsed$vars$types) && is.list(parsed$vars$types)) {
    parsed$vars$types <- unlist(parsed$vars$types, use.names = TRUE)
  }

  # Normalize scale reverse fields (YAML creates list for empty arrays)
  if (!is.null(parsed$scales) && length(parsed$scales) > 0) {
    for (i in seq_along(parsed$scales)) {
      if (!is.null(parsed$scales[[i]]$reverse)) {
        if (is.list(parsed$scales[[i]]$reverse)) {
          if (length(parsed$scales[[i]]$reverse) > 0) {
            parsed$scales[[i]]$reverse <- unlist(parsed$scales[[i]]$reverse, use.names = FALSE)
          } else {
            parsed$scales[[i]]$reverse <- character(0)
          }
        }
      }
    }
  }

  # Wrap using nicheCore constructor
  spec <- nicheCore::new_niche_spec(parsed)

  spec
}
