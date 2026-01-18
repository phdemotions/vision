test_that("validate_spec accepts valid spec", {
  path <- system.file("extdata", "valid_spec.yml", package = "vision")
  skip_if(path == "", "Test fixture not found")

  spec <- read_spec(path)
  result <- validate_spec(spec)

  expect_s3_class(result, "niche_spec")
  expect_identical(result, spec)
})

test_that("validate_spec fails on missing top-level field", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(),
    data = list()
    # Missing other required fields
  ))

  expect_error(
    validate_spec(spec),
    "spec\\$vars"
  )
})

test_that("validate_spec fails on invalid meta$title", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = c("Multiple", "Titles"),  # Invalid: should be character(1)
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character")),
    rules = list(),
    scales = list(),
    models = list(),
    outputs = list(root = "outputs")
  ))

  expect_error(
    validate_spec(spec),
    "spec\\$meta\\$title"
  )
})

test_that("validate_spec fails on missing meta$authors", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      # Missing authors
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character")),
    rules = list(),
    scales = list(),
    models = list(),
    outputs = list(root = "outputs")
  ))

  expect_error(
    validate_spec(spec),
    "spec\\$meta\\$authors"
  )
})

test_that("validate_spec accepts authors as character vector", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = c("Author1", "Author2"),
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character")),
    rules = list(),
    scales = list(),
    models = list(),
    outputs = list(root = "outputs")
  ))

  expect_silent(validate_spec(spec))
})

test_that("validate_spec accepts authors as list", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = list("Author1", "Author2"),
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character")),
    rules = list(),
    scales = list(),
    models = list(),
    outputs = list(root = "outputs")
  ))

  expect_silent(validate_spec(spec))
})

test_that("validate_spec fails on invalid variable type", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "invalid_type")),
    rules = list(),
    scales = list(),
    models = list(),
    outputs = list(root = "outputs")
  ))

  expect_error(
    validate_spec(spec),
    "Invalid variable types"
  )
})

test_that("validate_spec fails on scale missing required field", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character", item1 = "double")),
    rules = list(),
    scales = list(
      list(
        name = "scale1",
        items = "item1"
        # Missing min_items and composite
      )
    ),
    models = list(),
    outputs = list(root = "outputs")
  ))

  expect_error(
    validate_spec(spec),
    "spec\\$scales\\[\\[1\\]\\]\\$min_items"
  )
})

test_that("validate_spec fails on model missing required field", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character", y = "double", x = "double")),
    rules = list(),
    scales = list(),
    models = list(
      list(
        name = "model1"
        # Missing formula
      )
    ),
    outputs = list(root = "outputs")
  ))

  expect_error(
    validate_spec(spec),
    "spec\\$models\\[\\[1\\]\\]\\$formula"
  )
})

test_that("validate_spec fails on invalid outputs$root", {
  spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(types = c(id = "character")),
    rules = list(),
    scales = list(),
    models = list(),
    outputs = list(root = c("multiple", "roots"))  # Invalid
  ))

  expect_error(
    validate_spec(spec),
    "spec\\$outputs\\$root"
  )
})
