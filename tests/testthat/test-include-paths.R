# Disable sass cache
local_disable_cache()

test_that("single path works", {
  scss <- "@import \"need\""
  css <- sass(scss, options = sass_options(include_path = "test-include-path/"))

  actual <- read_utf8("test-include-path/_need.scss")
  class(actual) <- c("css", "html", "character")
  attr(actual, "html") <- TRUE

  expect_equal(css, actual)
})

test_that("multiple paths work", {
  scss <- "@import \"need\", \"need2\""
  css <- sass(
    scss,
    options = sass_options(include_path = c("test-include-path/", "test-include-path2/"))
  )

  css1 <- read_utf8("test-include-path/_need.scss")
  css2 <- read_utf8("test-include-path2/_need2.scss")
  actual <- paste0(c(css1, css2), collapse = "\n")
  class(actual) <- c("css", "html", "character")
  attr(actual, "html") <- TRUE

  expect_equal(css, actual)
})
