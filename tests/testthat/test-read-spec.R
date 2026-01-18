test_that("read_spec parses valid YAML", {
  path <- system.file("extdata", "valid_spec.yml", package = "vision")
  skip_if(path == "", "Test fixture not found")

  spec <- read_spec(path)

  expect_s3_class(spec, "niche_spec")
  expect_equal(spec$schema_version, "0.1.0")
  expect_equal(spec$meta$title, "Example Consumer Study")
  expect_true(!is.null(spec$source))
})

test_that("read_spec fails on nonexistent file", {
  expect_error(
    read_spec("nonexistent_file.yml"),
    "File does not exist"
  )
})

test_that("read_spec fails on unsupported extension", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("test", tmp)

  expect_error(
    read_spec(tmp),
    "Unsupported file extension"
  )

  unlink(tmp)
})

test_that("read_spec adds schema_version if missing", {
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "meta:",
    "  title: Test",
    "  authors: [Author]",
    "  created: 2026-01-01",
    "data:",
    "  sources: [data.csv]",
    "  id: id",
    "vars:",
    "  types:",
    "    id: character",
    "rules: {}",
    "scales: []",
    "models: []",
    "outputs:",
    "  root: outputs"
  ), tmp)

  spec <- read_spec(tmp)
  expect_equal(spec$schema_version, "0.1.0")

  unlink(tmp)
})
