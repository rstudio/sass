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
  on.exit(sass_options_set(NULL), add = TRUE)

  expect_identical(sass_options_get(), sass_options())

  old_options <- sass_options_set(precision = 10)
  expect_equal(
    sass_options_get(),
    sass_options(precision = 10),
    ignore_attr = TRUE
  )

  expect_equal(
    sass_options_get(precision = 11),
    sass_options(precision = 11),
    ignore_attr = TRUE
  )

  sass_options_set(old_options)
  expect_identical(sass_options_get(), sass_options())

  sass_options_set(precision = 12)
  opts <- sass_options_get()
  sass_options_set(precision = 13)
  sass_options_set(opts)
  expect_equal(
    sass_options_get(),
    sass_options(precision = 12),
    ignore_attr = TRUE
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

test_that("Global options work 2", {
  expect_identical(sass_options_get(), sass_options())
})
