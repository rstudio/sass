context('inclue paths')

test_that("single path works", {
  scss <- "@import 'need'"
  css <- compile(scss, opts(include_path = "include_path/"))

  actual <- paste0(readLines("include_path/_need.scss"), collapse = '\n')
  class(actual) <- c('css', class(actual))

  expect_equal(css, actual)
})

test_that("multiple paths work", {
  scss <- "@import 'need', 'need2'"
  css <- compile(
    scss,
    opts(include_path = c("include_path/", "include_path2/"))
  )

  css1 <- readLines("include_path/_need.scss")
  css2 <- readLines("include_path2/_need2.scss")
  actual <- paste0(c(css1, css2), collapse = '\n')
  class(actual) <- c('css', class(actual))

  expect_equal(css, actual)
})
