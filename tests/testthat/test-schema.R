test_that("schema_version_current returns expected version", {
  expect_equal(schema_version_current(), "0.1.0")
  expect_type(schema_version_current(), "character")
  expect_length(schema_version_current(), 1)
})
