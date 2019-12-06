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
  layer1 <- sass_layer(pre = "body{color: green}", deps = dep1)
  input1 <- list(layer1, "body{color: red}")
  expect_equal(htmlDependencies(sass(input1)), list(dep1))
  layer2 <- sass_layer(pre = "body{color: blue}", deps = dep2)
  input2 <- sass_layer_merge(layer1, layer2)
  expect_equal(htmlDependencies(sass(input2)), list(dep1, dep2))
})


test_that("sass() relays html dependencies attached to it's input sensibly", {
  # one html dependency, nested input
  input1 <- attachDependencies("body {color: red}", dep1)
  output1 <- sass(list(input1))
  output2 <- sass(list(list(input1)))
  output3 <- sass(list(list(input1), ".foo{color: red}", list(list(input1))))
  expect_equal(htmlDependencies(output1), list(dep1))
  expect_equal(htmlDependencies(output2), list(dep1))
  expect_equal(htmlDependencies(output3), list(dep1, dep1))
  # multiple html dependencies
  input2 <- attachDependencies("body {color: red}", dep2)
  output4 <- sass(list(list(input1), list(), list(list(input2))))
  expect_equal(htmlDependencies(output4), list(dep1, dep2))
})
