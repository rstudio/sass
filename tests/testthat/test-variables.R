context("variables")

# Disable sass cache
options(sass.cache = FALSE)

test_that("variables work", {
  css <- sass(sass_file("test-variables.scss"))
  expect_equal(
    strsplit(css, " ")[[1]][6],
    "Helvetica,"
  )
})
