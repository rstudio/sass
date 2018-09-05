context("compile")

test_that(".scss file compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n\nbar {\n  margin: 63px;\n}\n"
  class(expected) <- "sass"

  expect_equal(
    sass("test-compile.scss"),
    expected
  )
})

test_that("string input compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n"
  class(expected) <- "sass"

  expect_equal(
    sass("foo { margin: 122px * .3; }"),
    expected
  )
})

test_that("sass compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n\nbar {\n  margin: 63px;\n}\n"
  class(expected) <- "sass"

  expect_equal(
    sass("test-compile.sass", sass_options(indented_syntax = TRUE)),
    expected
  )
})
