context("compile")

test_that(".scss file compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n\nbar {\n  margin: 63px;\n}\n"
  class(expected) <- c('css', class(expected))

  expect_equal(
    compile("test-compile.scss"),
    expected
  )
})

test_that("string input compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n"
  class(expected) <- c('css', class(expected))

  expect_equal(
    compile("foo { margin: 122px * .3; }"),
    expected
  )
})

test_that("sass compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n\nbar {\n  margin: 63px;\n}\n"
  class(expected) <- c('css', class(expected))

  expect_equal(
    compile("test-compile.sass", opts(indented_syntax = TRUE)),
    expected
  )
})
