#' Validate a specification object
#'
#' Validates a niche_spec object against schema v0.1.0 requirements.
#' Performs structural validation only, not semantic domain validation.
#'
#' @param spec A niche_spec object from read_spec()
#'
#' @return The validated spec (unchanged)
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- read_spec("my_study.yml")
#' spec <- validate_spec(spec)
#' }
validate_spec <- function(spec) {
  # Ensure input is a niche_spec
  if (!inherits(spec, "niche_spec")) {
    nicheCore::niche_abort(
      "Input must be a niche_spec object",
      details = list(class = class(spec)),
      help = "Use read_spec() to create a niche_spec object"
    )
  }

  # Schema v0.1.0 validation rules
  # Check required top-level fields
  required_fields <- c("meta", "data", "vars", "rules", "scales", "models", "outputs", "schema_version")

  for (field in required_fields) {
    if (is.null(spec[[field]])) {
      nicheCore::niche_abort(
        "Missing required top-level field",
        details = list(field = paste0("spec$", field)),
        help = paste0("Add '", field, ":' section to your specification file")
      )
    }
  }

  # Validate schema_version
  if (!is.character(spec$schema_version) || length(spec$schema_version) != 1) {
    nicheCore::niche_abort(
      "Invalid schema_version",
      details = list(
        field = "spec$schema_version",
        value = spec$schema_version,
        expected = "character(1)"
      ),
      help = "Set schema_version to a single character string (e.g., '0.1.0')"
    )
  }

  # Validate meta
  validate_meta(spec$meta)

  # Validate data
  validate_data(spec$data)

  # Validate vars
  validate_vars(spec$vars)

  # Validate rules
  validate_rules(spec$rules)

  # Validate scales
  validate_scales(spec$scales)

  # Validate models
  validate_models(spec$models)

  # Validate outputs
  validate_outputs(spec$outputs)

  # Return unchanged spec
  spec
}

# Internal validators for each section

validate_meta <- function(meta) {
  # title: character(1)
  if (is.null(meta$title) || !is.character(meta$title) || length(meta$title) != 1) {
    nicheCore::niche_abort(
      "Invalid meta$title",
      details = list(
        field = "spec$meta$title",
        value = if (is.null(meta$title)) NULL else meta$title,
        expected = "character(1)"
      ),
      help = "Set meta.title to a single character string"
    )
  }

  # authors: character vector OR list of character scalars
  if (is.null(meta$authors)) {
    nicheCore::niche_abort(
      "Missing required field",
      details = list(field = "spec$meta$authors"),
      help = "Add meta.authors as a character vector or list"
    )
  }

  if (is.list(meta$authors)) {
    # Each element should be character(1)
    for (i in seq_along(meta$authors)) {
      if (!is.character(meta$authors[[i]]) || length(meta$authors[[i]]) != 1) {
        nicheCore::niche_abort(
          "Invalid authors list element",
          details = list(
            field = paste0("spec$meta$authors[[", i, "]]"),
            value = meta$authors[[i]],
            expected = "character(1)"
          ),
          help = "Each author in list must be a single character string"
        )
      }
    }
  } else if (!is.character(meta$authors)) {
    nicheCore::niche_abort(
      "Invalid meta$authors type",
      details = list(
        field = "spec$meta$authors",
        type = typeof(meta$authors),
        expected = "character vector or list"
      ),
      help = "Set meta.authors to a character vector or list of strings"
    )
  }

  # created: character(1) or Date
  if (is.null(meta$created)) {
    nicheCore::niche_abort(
      "Missing required field",
      details = list(field = "spec$meta$created"),
      help = "Add meta.created as a date string or Date object"
    )
  }

  if (!is.character(meta$created) && !inherits(meta$created, "Date")) {
    nicheCore::niche_abort(
      "Invalid meta$created type",
      details = list(
        field = "spec$meta$created",
        type = class(meta$created),
        expected = "character(1) or Date"
      ),
      help = "Set meta.created to a date string (e.g., '2026-01-18') or Date object"
    )
  }

  if (is.character(meta$created) && length(meta$created) != 1) {
    nicheCore::niche_abort(
      "Invalid meta$created length",
      details = list(
        field = "spec$meta$created",
        length = length(meta$created),
        expected = "character(1)"
      ),
      help = "Set meta.created to a single date string"
    )
  }

  invisible(NULL)
}

validate_data <- function(data) {
  # sources: character vector (paths may be placeholders)
  if (is.null(data$sources)) {
    nicheCore::niche_abort(
      "Missing required field",
      details = list(field = "spec$data$sources"),
      help = "Add data.sources as a character vector of file paths"
    )
  }

  if (!is.character(data$sources)) {
    nicheCore::niche_abort(
      "Invalid data$sources type",
      details = list(
        field = "spec$data$sources",
        type = typeof(data$sources),
        expected = "character vector"
      ),
      help = "Set data.sources to a character vector"
    )
  }

  # id: character(1)
  if (is.null(data$id) || !is.character(data$id) || length(data$id) != 1) {
    nicheCore::niche_abort(
      "Invalid data$id",
      details = list(
        field = "spec$data$id",
        value = if (is.null(data$id)) NULL else data$id,
        expected = "character(1)"
      ),
      help = "Set data.id to a single character string (name of ID column)"
    )
  }

  # joins: list (may be empty or missing; tolerate both)
  if (!is.null(data$joins) && !is.list(data$joins)) {
    nicheCore::niche_abort(
      "Invalid data$joins type",
      details = list(
        field = "spec$data$joins",
        type = typeof(data$joins),
        expected = "list"
      ),
      help = "Set data.joins to a list (or omit for empty joins)"
    )
  }

  invisible(NULL)
}

validate_vars <- function(vars) {
  # rename: named character vector (may be empty or missing)
  if (!is.null(vars$rename)) {
    if (!is.character(vars$rename)) {
      nicheCore::niche_abort(
        "Invalid vars$rename type",
        details = list(
          field = "spec$vars$rename",
          type = typeof(vars$rename),
          expected = "named character vector"
        ),
        help = "Set vars.rename to a named character vector"
      )
    }

    if (length(vars$rename) > 0 && is.null(names(vars$rename))) {
      nicheCore::niche_abort(
        "vars$rename must be named",
        details = list(field = "spec$vars$rename"),
        help = "Provide names for vars.rename (old_name: new_name)"
      )
    }
  }

  # types: named character vector with allowed values
  if (is.null(vars$types)) {
    nicheCore::niche_abort(
      "Missing required field",
      details = list(field = "spec$vars$types"),
      help = "Add vars.types as a named character vector"
    )
  }

  if (!is.character(vars$types)) {
    nicheCore::niche_abort(
      "Invalid vars$types type",
      details = list(
        field = "spec$vars$types",
        type = typeof(vars$types),
        expected = "named character vector"
      ),
      help = "Set vars.types to a named character vector"
    )
  }

  if (is.null(names(vars$types)) || any(names(vars$types) == "")) {
    nicheCore::niche_abort(
      "vars$types must be named",
      details = list(field = "spec$vars$types"),
      help = "Provide variable names for vars.types"
    )
  }

  # Check allowed type values
  allowed_types <- c("character", "double", "integer", "logical", "factor", "date", "datetime")
  invalid_types <- setdiff(vars$types, allowed_types)

  if (length(invalid_types) > 0) {
    nicheCore::niche_abort(
      "Invalid variable types",
      details = list(
        field = "spec$vars$types",
        invalid = invalid_types,
        allowed = allowed_types
      ),
      help = paste0("Use only allowed types: ", paste(allowed_types, collapse = ", "))
    )
  }

  invisible(NULL)
}

validate_rules <- function(rules) {
  # exclusions: list (may be empty or missing)
  if (!is.null(rules$exclusions) && !is.list(rules$exclusions)) {
    nicheCore::niche_abort(
      "Invalid rules$exclusions type",
      details = list(
        field = "spec$rules$exclusions",
        type = typeof(rules$exclusions),
        expected = "list"
      ),
      help = "Set rules.exclusions to a list"
    )
  }

  # missingness: list (may be empty or missing)
  if (!is.null(rules$missingness) && !is.list(rules$missingness)) {
    nicheCore::niche_abort(
      "Invalid rules$missingness type",
      details = list(
        field = "spec$rules$missingness",
        type = typeof(rules$missingness),
        expected = "list"
      ),
      help = "Set rules.missingness to a list"
    )
  }

  invisible(NULL)
}

validate_scales <- function(scales) {
  # scales: list (may be empty)
  if (!is.list(scales)) {
    nicheCore::niche_abort(
      "Invalid scales type",
      details = list(
        field = "spec$scales",
        type = typeof(scales),
        expected = "list"
      ),
      help = "Set scales to a list (or empty list for no scales)"
    )
  }

  # If non-empty, validate each scale
  for (i in seq_along(scales)) {
    scale <- scales[[i]]
    prefix <- paste0("spec$scales[[", i, "]]")

    # name: character(1)
    if (is.null(scale$name) || !is.character(scale$name) || length(scale$name) != 1) {
      nicheCore::niche_abort(
        "Invalid scale name",
        details = list(
          field = paste0(prefix, "$name"),
          value = if (is.null(scale$name)) NULL else scale$name,
          expected = "character(1)"
        ),
        help = "Each scale must have a name (single character string)"
      )
    }

    # items: character vector (>= 1)
    if (is.null(scale$items) || !is.character(scale$items) || length(scale$items) < 1) {
      nicheCore::niche_abort(
        "Invalid scale items",
        details = list(
          field = paste0(prefix, "$items"),
          value = if (is.null(scale$items)) NULL else scale$items,
          expected = "character vector with length >= 1"
        ),
        help = "Each scale must have at least one item"
      )
    }

    # reverse: character vector (optional)
    if (!is.null(scale$reverse) && !is.character(scale$reverse)) {
      nicheCore::niche_abort(
        "Invalid scale reverse",
        details = list(
          field = paste0(prefix, "$reverse"),
          type = typeof(scale$reverse),
          expected = "character vector"
        ),
        help = "Scale reverse must be a character vector (or omit if none)"
      )
    }

    # min_items: integer-like scalar >= 1
    if (is.null(scale$min_items)) {
      nicheCore::niche_abort(
        "Missing required scale field",
        details = list(field = paste0(prefix, "$min_items")),
        help = "Each scale must have min_items (integer >= 1)"
      )
    }

    if (!is.numeric(scale$min_items) || length(scale$min_items) != 1 || scale$min_items < 1) {
      nicheCore::niche_abort(
        "Invalid scale min_items",
        details = list(
          field = paste0(prefix, "$min_items"),
          value = scale$min_items,
          expected = "numeric scalar >= 1"
        ),
        help = "Set min_items to an integer >= 1"
      )
    }

    # composite: character(1)
    if (is.null(scale$composite) || !is.character(scale$composite) || length(scale$composite) != 1) {
      nicheCore::niche_abort(
        "Invalid scale composite",
        details = list(
          field = paste0(prefix, "$composite"),
          value = if (is.null(scale$composite)) NULL else scale$composite,
          expected = "character(1)"
        ),
        help = "Each scale must have composite method (single character string)"
      )
    }
  }

  invisible(NULL)
}

validate_models <- function(models) {
  # models: list (may be empty)
  if (!is.list(models)) {
    nicheCore::niche_abort(
      "Invalid models type",
      details = list(
        field = "spec$models",
        type = typeof(models),
        expected = "list"
      ),
      help = "Set models to a list (or empty list for no models)"
    )
  }

  # If non-empty, validate each model
  for (i in seq_along(models)) {
    model <- models[[i]]
    prefix <- paste0("spec$models[[", i, "]]")

    # name: character(1)
    if (is.null(model$name) || !is.character(model$name) || length(model$name) != 1) {
      nicheCore::niche_abort(
        "Invalid model name",
        details = list(
          field = paste0(prefix, "$name"),
          value = if (is.null(model$name)) NULL else model$name,
          expected = "character(1)"
        ),
        help = "Each model must have a name (single character string)"
      )
    }

    # formula: character(1)
    if (is.null(model$formula) || !is.character(model$formula) || length(model$formula) != 1) {
      nicheCore::niche_abort(
        "Invalid model formula",
        details = list(
          field = paste0(prefix, "$formula"),
          value = if (is.null(model$formula)) NULL else model$formula,
          expected = "character(1)"
        ),
        help = "Each model must have a formula (single character string)"
      )
    }

    # family: character(1) optional
    if (!is.null(model$family)) {
      if (!is.character(model$family) || length(model$family) != 1) {
        nicheCore::niche_abort(
          "Invalid model family",
          details = list(
            field = paste0(prefix, "$family"),
            value = model$family,
            expected = "character(1)"
          ),
          help = "Model family must be a single character string (or omit)"
        )
      }
    }

    # robust: logical(1) optional
    if (!is.null(model$robust)) {
      if (!is.logical(model$robust) || length(model$robust) != 1) {
        nicheCore::niche_abort(
          "Invalid model robust",
          details = list(
            field = paste0(prefix, "$robust"),
            value = model$robust,
            expected = "logical(1)"
          ),
          help = "Model robust must be TRUE or FALSE (or omit)"
        )
      }
    }
  }

  invisible(NULL)
}

validate_outputs <- function(outputs) {
  # root: character(1)
  if (is.null(outputs$root) || !is.character(outputs$root) || length(outputs$root) != 1) {
    nicheCore::niche_abort(
      "Invalid outputs$root",
      details = list(
        field = "spec$outputs$root",
        value = if (is.null(outputs$root)) NULL else outputs$root,
        expected = "character(1)"
      ),
      help = "Set outputs.root to a single directory path string"
    )
  }

  # render: list (may be empty or missing)
  if (!is.null(outputs$render) && !is.list(outputs$render)) {
    nicheCore::niche_abort(
      "Invalid outputs$render type",
      details = list(
        field = "spec$outputs$render",
        type = typeof(outputs$render),
        expected = "list"
      ),
      help = "Set outputs.render to a list (or omit for no rendering)"
    )
  }

  invisible(NULL)
}
