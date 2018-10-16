context("inclue paths")

# Disable sass cache
options(sass.cache = FALSE)

test_that("single path works", {
  scss <- "@import \"need\""
  css <- sass(scss, options = sass_options(include_path = "test-include-path/"))

  actual <- paste0(readLines("test-include-path/_need.scss"), collapse = "\n")
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

  css1 <- readLines("test-include-path/_need.scss")
  css2 <- readLines("test-include-path2/_need2.scss")
  actual <- paste0(c(css1, css2), collapse = "\n")
  class(actual) <- c("css", "html", "character")
  attr(actual, "html") <- TRUE

  expect_equal(css, actual)
})
