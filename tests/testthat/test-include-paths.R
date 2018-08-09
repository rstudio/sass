context('inclue paths')

test_that("single path works", {
  scss <- "@import 'need'"
  css <- compile_sass(text = scss, options = opts(include_path = "test-include_path/"))

  actual <- paste0(readLines("test-include_path/_need.scss"), collapse = '\n')
  class(actual) <- c('css', class(actual))

  expect_equal(css, actual)
})

test_that("multiple paths work", {
  scss <- "@import 'need', 'need2'"
  css <- compile_sass(
    text = scss,
    options = opts(include_path = c("test-include_path/", "test-include_path2/"))
  )

  css1 <- readLines("test-include_path/_need.scss")
  css2 <- readLines("test-include_path2/_need2.scss")
  actual <- paste0(c(css1, css2), collapse = '\n')
  class(actual) <- c('css', class(actual))

  expect_equal(css, actual)
})
