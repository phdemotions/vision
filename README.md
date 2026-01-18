# vision

<!-- badges: start -->
<!-- badges: end -->

## Overview

`vision` provides spec-first workflow infrastructure for consumer psychology research. It parses and validates YAML or JSON specification files, constructs auditable analysis recipes, and enforces schema compliance. `vision` does **not** perform data ingestion, modeling, or reporting—it focuses solely on schema validation and recipe construction to ensure reproducible, audit-ready research workflows.

`vision` is part of the **niche** R universe for reproducible consumer research.

## Installation

You can install the development version of vision from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("username/vision")
```

## Minimal Example

Here's a complete end-to-end workflow using `vision`:

``` r
library(vision)

# 1. Create a specification template
template_path <- tempfile(fileext = ".yml")
write_spec_template(template_path)

# 2. Read the specification
spec <- read_spec(template_path)

# 3. Validate the specification against schema v0.1.0
spec <- validate_spec(spec)

# 4. Build an auditable recipe
recipe <- build_recipe(spec)

# 5. Write the recipe to disk
write_recipe(recipe)

# The recipe is now saved as JSON in the audit directory
# and can be used by downstream packages for analysis execution
```

## Key Functions

- `schema_version_current()`: Returns the current schema version ("0.1.0")
- `read_spec(path)`: Parse YAML/JSON specification files
- `validate_spec(spec)`: Validate against schema v0.1.0 requirements
- `build_recipe(spec)`: Construct auditable analysis recipe with deterministic hashing
- `write_recipe(recipe, path)`: Serialize recipe to canonical JSON
- `write_spec_template(path)`: Generate a minimal compliant YAML template

## What vision Does NOT Do

`vision` is intentionally minimal and focused:

- ❌ Does not read or validate data files
- ❌ Does not score scales or compute composites
- ❌ Does not run statistical models
- ❌ Does not produce tables, figures, or reports

For complete analysis workflows, use `vision` in combination with other packages in the niche universe.

## Schema v0.1.0

`vision` validates specifications against schema version 0.1.0, which requires:

**Top-level fields:**
- `meta`: Study metadata (title, authors, created date)
- `data`: Data sources and ID column specification
- `vars`: Variable renaming and type specifications
- `rules`: Exclusion and missingness handling rules
- `scales`: Scale definitions with items and compositing methods
- `models`: Statistical model specifications
- `outputs`: Output directory configuration

See the vignette for detailed schema requirements and examples.

## License

MIT
