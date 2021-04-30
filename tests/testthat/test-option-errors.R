# Disable sass cache
local_disable_cache()

test_that("unnamed options fail", {
  sass_options <- sass_options()
  names(sass_options) <- NULL
  expect_error(
    sass("foo { margin: 122px * .3; }", options = sass_options),
    "No named options"
  )
})

test_that("too few options fail", {
  sass_options <- sass_options()
  sass_options$precision <- NULL
  expect_error(
    sass("foo { margin: 122px * .3; }", options = sass_options),
    "missing"
  )
})

test_that("too many options fail", {
  sass_options <- sass_options()
  sass_options$new <- "hello"
  expect_error(
    sass("foo { margin: 122px * .3; }", options = sass_options),
    "unsupported"
  )
})

test_that("wrong options fail", {
  sass_options <- sass_options()
  sass_options$precision <- NULL
  sass_options$blah <- "hello"
  expect_error(
    sass("foo { margin: 122px * .3; }", options = sass_options),
    "precision"
  )
})

test_that("wrong type fails", {
  sass_options <- sass_options()
  sass_options$precision <- "hello"
  expect_error(
    sass("foo { margin: 122px * .3; }", options = sass_options),
    "Invalid type for precision"
  )
})

test_that("Global options work", {
  expect_identical(
    sass_options_get(),
    sass_options()
  )

  opts <- sass_options(precision = 10)
  sass_options_set(opts)
  expect_identical(sass_options_get(), opts)

  sass_options_set(NULL)
  expect_identical(sass_options_get(), sass_options())

  expect_identical(
    sass_options_get(precision = 11),
    sass_options(precision = 11)
  )

  expect_error(
    sass_options_set(1),
    "sass_options"
  )
  expect_error(
    sass_options_get(foo = "bar"),
    "foo"
  )
})
