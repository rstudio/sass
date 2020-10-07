context("layers")

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


test_that("as_sass_layer_list() produces a list", {

  sll_ <- function(x) {
    structure(x, class = "sass_layer_list")
  }

  # list input
  expect_equivalent(
    as_sass_layer_list(blue),
    sll_(blue)
  )

  # character input
  expect_equivalent(
    as_sass_layer_list(body_rule),
    sll_(list(body_rule))
  )
  expect_equivalent(
    as_sass_layer_list(c(body_rule, body_rule)),
    sll_(list(body_rule, body_rule))
  )
  expect_equivalent(
    as_sass_layer_list(c(body_rule, B = body_rule)),
    sll_(list(body_rule, B = body_rule))
  )

  expect_error(
    as_sass_layer_list(
      sass_removable(body_rule)
    ),
    "must be nested"
  )

  # complicated list input
  items <- append(
    c(
      blue,
      red
    ),
    list(
      body = sass_removable(body_rule),
      green
    )
  )
  expect_equivalent(
    as_sass_layer_list(items),
    sll_(items)
  )
})
