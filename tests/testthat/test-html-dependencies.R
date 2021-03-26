# Disable sass cache
local_disable_cache()

library(htmltools)
dep1 <- htmlDependency(
  name = "fake1",
  version = "1.0.0",
  src = ""
)
dep2 <- htmlDependency(
  name = "fake2",
  version = "1.0.0",
  src = ""
)

test_that("sass()/as_sass() relay html dependencies", {
  scss <- attachDependencies(list("body{color: green}"), dep1)
  # Test that the print methods also relay
  expect_snapshot(sass(scss), cran = TRUE)
  expect_snapshot(as_sass(scss), cran = TRUE)
  tmpcss <- tempfile(fileext = ".css")
  on.exit(unlink(tmpcss), add = TRUE)
  expect_equal(htmlDependencies(sass(scss)), list(dep1))
})

test_that("sass() relays sass_layer()'s html dependencies", {
  layer1 <- sass_layer(defaults = "body{color: green}", html_deps = dep1)
  input1 <- list(layer1, "body{color: red}")
  expect_equal(htmlDependencies(sass(input1)), list(dep1))
  layer2 <- sass_layer(defaults = "body{color: blue}", html_deps = dep2)
  input2 <- sass_bundle(layer1, layer2)
  expect_equal(htmlDependencies(sass(input2)), list(dep1, dep2))
})
