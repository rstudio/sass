context("layers")

# Disable sass cache
local_disable_cache()

body_rule <- "body { background-color: $color; color: my_invert($color); }"

blue <- list(color = "blue !default")
red <- list(color = "red !default")
green <- list(color = "green !default")
core <- sass_layer(
  defaults = blue,
  declarations = "@function my_invert($color, $amount: 100%) {
    $inverse: change-color($color, $hue: hue($color) + 180);
    @return mix($inverse, $color, $amount);
  }",
  rules = body_rule
)

test_that("sass_layer is equivalent to sass", {
  expect_equivalent(
    sass(core),
    sass(list(blue, "body { background-color: $color; color: yellow; }"))
  )
})
