# Disable sass cache
local_disable_cache()

test_that("nesting works", {
  expected <- read_utf8("test-nesting-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-nesting-input.scss"))

  expect_equal(css, expected)
})
