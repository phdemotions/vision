test_that("write_recipe creates JSON file in default location", {
  path <- system.file("extdata", "valid_spec.yml", package = "vision")
  skip_if(path == "", "Test fixture not found")

  spec <- read_spec(path)

  # Use tempdir for outputs
  spec$outputs$root <- tempdir()

  recipe <- build_recipe(spec)
  written_path <- write_recipe(recipe)

  expect_true(file.exists(written_path))
  expect_match(written_path, "recipe\\.json$")
  expect_match(written_path, "audit")

  # Clean up
  unlink(written_path)
})

test_that("write_recipe uses custom path when provided", {
  path <- system.file("extdata", "valid_spec.yml", package = "vision")
  skip_if(path == "", "Test fixture not found")

  spec <- read_spec(path)
  spec$outputs$root <- tempdir()
  recipe <- build_recipe(spec)

  custom_path <- file.path(tempdir(), "custom_recipe.json")
  written_path <- write_recipe(recipe, path = custom_path)

  expect_equal(written_path, custom_path)
  expect_true(file.exists(custom_path))

  # Verify it's valid JSON
  content <- jsonlite::fromJSON(custom_path)
  expect_type(content, "list")

  # Clean up
  unlink(custom_path)
})

test_that("write_recipe creates parent directories", {
  path <- system.file("extdata", "valid_spec.yml", package = "vision")
  skip_if(path == "", "Test fixture not found")

  spec <- read_spec(path)
  spec$outputs$root <- tempdir()
  recipe <- build_recipe(spec)

  nested_path <- file.path(tempdir(), "a", "b", "c", "recipe.json")
  written_path <- write_recipe(recipe, path = nested_path)

  expect_true(file.exists(written_path))

  # Clean up
  unlink(dirname(nested_path), recursive = TRUE)
})
