context("variables")

test_that("variables work", {
  css <- compile("test-variables.scss")
  expect_equal(
    strsplit(css, ' ')[[1]][6],
    "Helvetica,"
  )
})
