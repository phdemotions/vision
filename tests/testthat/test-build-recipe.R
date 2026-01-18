test_that("build_recipe creates valid recipe", {
  path <- system.file("extdata", "valid_spec.yml", package = "vision")
  skip_if(path == "", "Test fixture not found")

  spec <- read_spec(path)
  recipe <- build_recipe(spec)

  expect_s3_class(recipe, "niche_recipe")
  expect_equal(recipe$schema_version, "0.1.0")
  expect_type(recipe$spec_hash, "character")
  expect_length(recipe$spec_hash, 1)
  expect_type(recipe$defaults_applied, "logical")
  expect_length(recipe$defaults_applied, 1)
  expect_type(recipe$steps, "list")
  expect_length(recipe$steps, 0)  # Empty in phase 1
  expect_type(recipe$outputs, "list")
  expect_s3_class(recipe$created, "POSIXct")
})

test_that("build_recipe is deterministic", {
  # Create two specs with same content but different ordering
  spec1 <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = c("Author1", "Author2"),
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(
      types = c(var1 = "double", var2 = "integer", id = "character")
    ),
    rules = list(),
    scales = list(
      list(name = "scale_b", items = "var1", min_items = 1, composite = "mean"),
      list(name = "scale_a", items = "var2", min_items = 1, composite = "mean")
    ),
    models = list(),
    outputs = list(root = tempdir())
  ))

  spec2 <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = list("Author1", "Author2"),  # List instead of vector
      created = "2026-01-01"
    ),
    data = list(sources = "data.csv", id = "id"),
    vars = list(
      types = c(id = "character", var2 = "integer", var1 = "double")  # Different order
    ),
    rules = list(),
    scales = list(
      list(name = "scale_a", items = "var2", min_items = 1, composite = "mean"),
      list(name = "scale_b", items = "var1", min_items = 1, composite = "mean")  # Different order
    ),
    models = list(),
    outputs = list(root = tempdir())
  ))

  recipe1 <- build_recipe(spec1)
  recipe2 <- build_recipe(spec2)

  # Hashes should be identical due to canonicalization
  expect_equal(recipe1$spec_hash, recipe2$spec_hash)
})

test_that("build_recipe tracks whether defaults were applied", {
  # Spec with missing optional fields
  spec_with_defaults <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(
      sources = "data.csv",
      id = "id"
      # joins missing - should get default
    ),
    vars = list(
      # rename missing - should get default
      types = c(id = "character")
    ),
    rules = list(),  # exclusions and missingness missing
    scales = list(),
    models = list(),
    outputs = list(
      root = tempdir()
      # render missing - should get default
    )
  ))

  recipe <- build_recipe(spec_with_defaults)

  # defaults_applied should be TRUE when defaults were applied
  expect_type(recipe$defaults_applied, "logical")
  expect_length(recipe$defaults_applied, 1)
  expect_true(recipe$defaults_applied)

  # Spec with all optional fields provided
  spec_no_defaults <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(
      title = "Test",
      authors = "Author",
      created = "2026-01-01"
    ),
    data = list(
      sources = "data.csv",
      id = "id",
      joins = list()
    ),
    vars = list(
      rename = stats::setNames(character(0), character(0)),
      types = c(id = "character")
    ),
    rules = list(
      exclusions = list(),
      missingness = list()
    ),
    scales = list(),
    models = list(),
    outputs = list(
      root = tempdir(),
      render = list()
    )
  ))

  recipe2 <- build_recipe(spec_no_defaults)
  expect_false(recipe2$defaults_applied)
})

test_that("build_recipe creates output directory paths", {
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
    outputs = list(root = tempdir())
  ))

  recipe <- build_recipe(spec)

  expect_type(recipe$outputs, "list")
  expect_true(length(recipe$outputs) > 0)
  # Should have paths like 'audit', 'data', 'figures', etc.
  # Exact structure defined by nicheCore
})

test_that("build_recipe validates spec first", {
  invalid_spec <- nicheCore::new_niche_spec(list(
    schema_version = "0.1.0",
    meta = list(title = "Test")
    # Missing required fields like data, vars, etc
  ))

  expect_error(
    build_recipe(invalid_spec),
    "Missing required top-level field"
  )
})
