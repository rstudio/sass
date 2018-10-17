context("nesting")

# Disable sass cache
options(sass.cache = FALSE)

test_that("nesting works", {
  expected <- paste0(readLines("test-nesting-expected.css"), collapse = "\n")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-nesting-input.scss"))

  expect_equal(css, expected)
})
