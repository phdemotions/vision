test_that("write_spec_template creates valid YAML file", {
  tmp <- tempfile(fileext = ".yml")
  written_path <- write_spec_template(tmp)

  expect_equal(written_path, tmp)
  expect_true(file.exists(tmp))

  # Verify template can be read and validated
  spec <- read_spec(tmp)
  expect_s3_class(spec, "niche_spec")

  # Update root to tempdir for build_recipe
  spec$outputs$root <- tempdir()

  # Should be able to build recipe from template
  expect_silent({
    recipe <- build_recipe(spec)
  })

  # Clean up
  unlink(tmp)
})

test_that("write_spec_template creates parent directories", {
  nested_path <- file.path(tempdir(), "a", "b", "template.yml")
  written_path <- write_spec_template(nested_path)

  expect_true(file.exists(written_path))

  # Clean up
  unlink(file.path(tempdir(), "a"), recursive = TRUE)
})
