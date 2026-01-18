#' Write a specification template
#'
#' Creates a minimal but complete YAML specification template that conforms to
#' schema v0.1.0 and can be used as a starting point for new studies.
#'
#' @param path Character scalar path for output YAML file
#'
#' @return The written path (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' write_spec_template("my_study.yml")
#' }
write_spec_template <- function(path) {
  # Ensure parent directory exists
  ensure_parent_dir(path)

  # Create minimal template that satisfies schema v0.1.0
  template <- c(
    "# Niche Specification Template (Schema v0.1.0)",
    "# Replace placeholders with your study-specific values",
    "",
    "schema_version: \"0.1.0\"",
    "",
    "meta:",
    "  title: \"Study Title\"",
    "  authors:",
    "    - \"Author Name\"",
    "  created: \"2026-01-18\"",
    "",
    "data:",
    "  sources:",
    "    - \"data/survey_wave1.csv\"",
    "  id: \"participant_id\"",
    "  # joins: []  # optional - list of join specifications",
    "",
    "vars:",
    "  # rename:  # optional - map old names to new names",
    "  #   old_name: new_name",
    "  types:",
    "    participant_id: character",
    "    age: integer",
    "    condition: factor",
    "",
    "rules: {}  # or provide exclusions and missingness lists",
    "",
    "scales: []  # or provide scale definitions",
    "",
    "models: []  # or provide model definitions",
    "",
    "outputs:",
    "  root: \"outputs\"",
    "  # render: []  # optional - rendering specifications"
  )

  # Write to file
  writeLines(template, path)

  invisible(path)
}
