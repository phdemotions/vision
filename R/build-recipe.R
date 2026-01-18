#' Build an analysis recipe from a validated specification
#'
#' Constructs a niche_recipe object from a validated niche_spec. Applies defaults,
#' canonicalizes for determinism, computes spec hash, and creates output directory
#' structure.
#'
#' @param spec A niche_spec object (will be validated automatically)
#'
#' @return A niche_recipe object ready for execution or serialization
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- read_spec("my_study.yml")
#' recipe <- build_recipe(spec)
#' }
build_recipe <- function(spec) {
  # Validate spec first
  spec <- validate_spec(spec)

  # Track whether defaults were applied
  defaults_tracking <- list()

  # Apply defaults to optional fields
  result <- apply_defaults(spec, defaults_tracking)
  spec <- result$spec
  defaults_tracking <- result$defaults_applied

  # Flag indicating whether any defaults were applied
  defaults_applied <- length(defaults_tracking) > 0

  # Canonicalize for deterministic hashing
  # This includes:
  # - Ensuring meta$authors is character vector
  # - Sorting vars$rename and vars$types by names
  # - Sorting scales and models by name
  # - Ensuring reverse is character(0) if missing in scales
  canonical_spec <- canonicalize_spec(spec)

  # Compute spec hash
  # Hash includes the entire canonicalized spec to ensure reproducibility
  # Any change to user inputs (fields, values, ordering after canonicalization)
  # will produce a different hash
  spec_hash <- nicheCore::hash_object(canonical_spec)

  # Create output directory structure
  output_paths <- nicheCore::niche_output_paths(root = spec$outputs$root)

  # Construct recipe object
  recipe_data <- list(
    schema_version = spec$schema_version,
    spec_hash = spec_hash,
    defaults_applied = defaults_applied,  # logical(1)
    steps = list(),  # Empty in phase 1
    outputs = output_paths,
    created = Sys.time()  # POSIXct
  )

  # Use nicheCore constructor
  recipe <- nicheCore::new_niche_recipe(recipe_data)

  recipe
}
