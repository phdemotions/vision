# Internal defaults - single source of truth
# Only apply these to OPTIONAL fields
.vision_defaults <- list(
  data = list(
    joins = list()
  ),
  vars = list(
    rename = stats::setNames(character(0), character(0))
  ),
  rules = list(
    exclusions = list(),
    missingness = list()
  ),
  outputs = list(
    render = list()
  ),
  scales = list(
    reverse = character(0)
  )
)

# Internal helper to normalize paths
normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Internal helper to ensure parent directory exists
ensure_parent_dir <- function(path) {
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

# Internal helper to apply defaults and track them
apply_defaults <- function(spec, defaults_applied) {
  # Apply data$joins if missing
  if (is.null(spec$data$joins)) {
    spec$data$joins <- .vision_defaults$data$joins
    defaults_applied[["data$joins"]] <- .vision_defaults$data$joins
  }

  # Apply vars$rename if missing
  if (is.null(spec$vars$rename)) {
    spec$vars$rename <- .vision_defaults$vars$rename
    defaults_applied[["vars$rename"]] <- .vision_defaults$vars$rename
  }

  # Apply rules$exclusions if missing
  if (is.null(spec$rules$exclusions)) {
    spec$rules$exclusions <- .vision_defaults$rules$exclusions
    defaults_applied[["rules$exclusions"]] <- .vision_defaults$rules$exclusions
  }

  # Apply rules$missingness if missing
  if (is.null(spec$rules$missingness)) {
    spec$rules$missingness <- .vision_defaults$rules$missingness
    defaults_applied[["rules$missingness"]] <- .vision_defaults$rules$missingness
  }

  # Apply outputs$render if missing
  if (is.null(spec$outputs$render)) {
    spec$outputs$render <- .vision_defaults$outputs$render
    defaults_applied[["outputs$render"]] <- .vision_defaults$outputs$render
  }

  list(spec = spec, defaults_applied = defaults_applied)
}

# Internal helper to canonicalize spec for deterministic hashing
canonicalize_spec <- function(spec) {
  canonical <- spec

  # Ensure meta$authors is a character vector
  if (!is.null(canonical$meta$authors)) {
    if (is.list(canonical$meta$authors)) {
      canonical$meta$authors <- unlist(canonical$meta$authors, use.names = FALSE)
    }
  }

  # Sort vars$rename and vars$types by names
  if (!is.null(canonical$vars$rename) && length(canonical$vars$rename) > 0) {
    canonical$vars$rename <- canonical$vars$rename[order(names(canonical$vars$rename))]
  }
  if (!is.null(canonical$vars$types) && length(canonical$vars$types) > 0) {
    canonical$vars$types <- canonical$vars$types[order(names(canonical$vars$types))]
  }

  # Sort scales by name
  if (!is.null(canonical$scales) && length(canonical$scales) > 0) {
    scale_names <- vapply(canonical$scales, function(s) s$name, character(1))
    canonical$scales <- canonical$scales[order(scale_names)]
  }

  # Sort models by name
  if (!is.null(canonical$models) && length(canonical$models) > 0) {
    model_names <- vapply(canonical$models, function(m) m$name, character(1))
    canonical$models <- canonical$models[order(model_names)]
  }

  # Ensure reverse is character vector in each scale
  if (!is.null(canonical$scales) && length(canonical$scales) > 0) {
    for (i in seq_along(canonical$scales)) {
      if (is.null(canonical$scales[[i]]$reverse)) {
        canonical$scales[[i]]$reverse <- character(0)
      }
    }
  }

  canonical
}
