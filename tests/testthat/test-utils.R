# Disable sass cache
local_disable_cache()

test_that("has_any_name finds names", {

  expect_true(has_any_name(list(a = 4, 5)))
  expect_true(has_any_name(list(4, a = 5)))
  expect_true(has_any_name(list(a = 4, b = 5)))

  expect_false(has_any_name(list(4, 5)))
})

test_that("has_any_name_recursive finds names", {

  expect_true(has_any_name_recursive(list(a = 4, 5)))
  expect_true(has_any_name_recursive(list(4, a = 5)))
  expect_true(has_any_name_recursive(list(a = 4, b = 5)))

  expect_true(has_any_name_recursive(list(list(list(list(list(c(a = 4))))))))
  expect_true(has_any_name_recursive(list(1,2, list(a = 4, 5), 4)))
  expect_true(has_any_name_recursive(list(1,2, list(4, a = 5), 4)))

  expect_false(has_any_name_recursive(4))
  expect_false(has_any_name_recursive(list(list(list(list(list(4)))))))
  expect_false(has_any_name_recursive(list(list(list(list(list(4, 5, 6, 7)))))))
})


test_that("join_non_null_values combines null elements as expected", {

  obj <- sass_bundle(
    ".rule-a {}", # sass rule
    list(color = "blue"), # sass default
    sass_layer(mixins = ".rule-b {}"),
    ".rule-c {}", # sass rule
  )
  expected <- sass_layer(
    defaults = list(color = "blue"),
    mixins = ".rule-b {}",
    rules = list(".rule-a {}", ".rule-c {}")
  )

  expect_equal(as_sass_layer(obj), as_sass_layer(expected))
  expect_snapshot(obj)
})


test_that("bundle like objects can be detected", {

  sass_item <- sass_layer("body {}")
  expect_equal(find_bundle_or_layer(list(list(list(4, 5, 6, list(sass_item), 7)))), sass_item)
  expect_equal(find_bundle_or_layer(list(list(list(4, 5, 6, sass_item, 7)))), sass_item)
  expect_equal(find_bundle_or_layer(list(list(list(4, 5, 6, sass_item, 7)))), sass_item)
  expect_equal(find_bundle_or_layer(list(list(list(4, 5, 6, as_sass_layer(sass_item), 7)))), as_sass_layer(sass_item))

  expect_equal(find_bundle_or_layer(list(list(list(4, 5, 6, 10, 7)))), NULL)

  my_layer <- sass_layer(rules = "body {}")
  expect_error(
    sass_layer(defaults = list(1, 2, my_layer, 4)),
    "`sass_layer(defaults)` can not contain", fixed = TRUE
  )
  expect_error(
    sass_layer(functions = list(1, 2, my_layer, 4)),
    "sass_layer(functions)` can not contain", fixed = TRUE
  )
  expect_error(
    sass_layer(mixins = list(1, 2, my_layer, 4)),
    "sass_layer(mixins)` can not contain", fixed = TRUE
  )
  expect_error(
    sass_layer(rules = list(1, 2, my_layer, 4)),
    "sass_layer(rules)` can not contain", fixed = TRUE
  )
})
