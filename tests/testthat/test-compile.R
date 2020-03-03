context("compile")

# Disable sass cache
options(sass.cache = FALSE)

test_that(".scss file compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n\nbar {\n  margin: 63px;\n}\n"
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  expect_equal(
    sass(sass_file("test-compile.scss")),
    expected
  )
})

test_that("string input compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n"
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  expect_equal(
    sass("foo { margin: 122px * .3; }"),
    expected
  )
})

test_that("character vector input compiles", {
  input <- c(".foo {\n  color: red;\n}\n", ".bar {\n  background-color: blue;\n}\n")
  expected <- paste(input, collapse = "\n")
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  expect_equal(sass(input), expected)
  expect_equal(sass(as.list(input)), expected)
})

test_that("named character vector throws warning", {
  expect_warning(sass(c(var = "foo{color: red}")), "named")
})

test_that("sass compiles", {
  expected <- "foo {\n  margin: 36.6px;\n}\n\nbar {\n  margin: 63px;\n}\n"
  class(expected) <- c("css", "html", "character")
  attr(expected, "html") <- TRUE

  expect_equal(
    sass(sass_file("test-compile.sass"), sass_options(indented_syntax = TRUE)),
    expected
  )
})
