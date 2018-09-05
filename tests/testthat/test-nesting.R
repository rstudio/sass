context("nesting")

test_that("nesting works", {
  expected <- paste0(readLines("test-nesting-expected.css"), collapse = "\n")
  class(expected) <- "sass"

  css <- sass("test-nesting-input.scss")

  expect_equal(css, expected)
})
