context('inclue paths')

test_that("single path works", {
  scss <- "@import 'need'"
  css <- compile_sass(text = scss, options = sass_options(include_path = "test-include-path/"))

  actual <- paste0(readLines("test-include-path/_need.scss"), collapse = '\n')
  class(actual) <- c('css', class(actual))

  expect_equal(css, actual)
})

test_that("multiple paths work", {
  scss <- "@import 'need', 'need2'"
  css <- compile_sass(
    text = scss,
    options = sass_options(include_path = c("test-include-path/", "test-include-path2/"))
  )

  css1 <- readLines("test-include-path/_need.scss")
  css2 <- readLines("test-include-path2/_need2.scss")
  actual <- paste0(c(css1, css2), collapse = '\n')
  class(actual) <- c('css', class(actual))

  expect_equal(css, actual)
})
