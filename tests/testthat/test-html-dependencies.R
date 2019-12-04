context("html-dependencies")

library(htmltools)
dep1 <- htmlDependency(
  name = "fake1",
  version = "1.0.0",
  src = system.file("tests", "testthat", package = "sass"),
  stylesheet = "test-nesting-expected.css",
  all_files = FALSE
)
dep2 <- htmlDependency(
  name = "fake2",
  version = "1.0.0",
  src = system.file("tests", "testthat", package = "sass"),
  stylesheet = "test-nesting-expected.css",
  all_files = FALSE
)

test_that("sass() relays sass_layer()'s html dependencies", {

  input <- list(
    sass_layer(pre = "body{color: green}", deps = dep1),
    "body{color: red}"
  )
  expect_equal(htmlDependencies(sass(input)), list(dep1))

  # if output is written to a file, the dependency should be copied
  # over to that output directory
  tmp <- tempfile(fileext = ".css")
  sass(output = tmp, input)
  opts <- options(htmltools.dir.version = FALSE)
  on.exit(options(opts), add = TRUE)
  dep_file <- file.path(dirname(tmp), dep1$name, dep1$stylesheet)
  expect_true(file.exists(dep_file))
})


test_that("sass() relays html dependencies attached to it's input sensibly", {
  # one html dependency, nested input
  input1 <- attachDependencies("body {color: red}", dep1)
  output1 <- sass(list(input1))
  output2 <- sass(list(list(input1)))
  output3 <- sass(list(list(input1), ".foo{color: red}", list(list(input1))))
  expect_equal(htmlDependencies(output1), list(dep1))
  expect_equal(htmlDependencies(output2), list(dep1))
  expect_equal(htmlDependencies(output3), list(dep1))
  # multiple html dependencies
  input2 <- attachDependencies("body {color: red}", dep2)
  output4 <- sass(list(list(input1), list(), list(list(input2))))
  expect_equal(htmlDependencies(output4), list(dep1, dep2))
})
