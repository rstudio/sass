context("layers")

blue <- list(color = "blue !default")
red <- list(color = "red !default")
green <- list(color = "green !default")
core <- sass_layer(pre = blue, post = "body { color: $color; }")

test_that("sass_layer is equivalent to sass", {
  expect_equivalent(
    sass(core),
    sass(list(blue, "body { color: $color; }"))
  )
})

test_that("sass_layer_stack() works as intended", {
  red_layer <- sass_layer(red, ":root{ --color: #{$color}; }")
  expect_equivalent(
    sass(list(red, core, ":root{ --color: #{$color}; }")),
    sass(sass_layer_stack(core, red_layer))
  )
})
