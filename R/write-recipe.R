#' Write a recipe to disk
#'
#' Serializes a niche_recipe object to JSON format in the audit directory.
#'
#' @param recipe A niche_recipe object from build_recipe()
#' @param path Optional path for output. Defaults to audit/recipe.json
#'
#' @return The written path (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- read_spec("my_study.yml")
#' recipe <- build_recipe(spec)
#' write_recipe(recipe)
#' }
write_recipe <- function(recipe, path = NULL) {
  # Validate recipe
  recipe <- nicheCore::validate_niche_recipe(recipe)

  # Determine output path
  if (is.null(path)) {
    path <- file.path(recipe$outputs$audit, "recipe.json")
  }

  # Ensure parent directory exists
  ensure_parent_dir(path)

  # Write using nicheCore canonical JSON writer
  # Unclass to convert S3 object to plain list for JSON serialization
  nicheCore::write_audit_json(unclass(recipe), path)

  invisible(path)
}
