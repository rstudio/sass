test_that("unicode variables work", {
  local_disable_cache()
  expected <- read_utf8("test-unicode-var-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-var-input.scss"))

  expect_equal(css, expected)
})

test_that("unicode css works with cache enabled", {
  expected <- read_utf8("test-unicode-css-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-css-input.scss"))

  expect_equal(Encoding(css), "UTF-8")
  expect_equal(css, expected)
})

test_that("unicode css works with cache disabled", {
  local_disable_cache()
  expected <- read_utf8("test-unicode-css-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-css-input.scss"))

  expect_equal(Encoding(css), "UTF-8")
  expect_equal(css, expected)
})

test_that("unicode bom", {
  expected <- read_utf8("test-unicode-bom-expected.css")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  css <- sass(sass_file("test-unicode-bom-input.scss"))

  expect_equal(css, expected)
})
