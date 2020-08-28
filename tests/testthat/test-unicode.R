context("unicode")

# Disable sass cache
local_disable_cache()

test_that("unicode variables work", {
  expected <- read_utf8("test-unicode-var-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-var-input.scss"))

  expect_equal(css, expected)
})

test_that("unicode css works", {
  expected <- read_utf8("test-unicode-css-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-css-input.scss"))

  expect_equal(css, expected)
})

test_that("unicode bom", {
  expected <- read_utf8("test-unicode-bom-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-bom-input.scss"))

  expect_equal(css, expected)
})
