context("utils")

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
    ".first {}", # sass rule
    list(color = "blue"), # sass default
    sass_layer(declarations = ".declarations {}"),
    ".second {}", # sass rule
  )
  expected <- sass_layer(
    defaults = list(color = "blue"),
    declarations = ".declarations {}",
    rules = list(".first {}", ".second {}")
  )


  expect_equal(as_sass_layer(obj), as_sass_layer(expected))
  expect_equal(
    capture.output(print(obj)),
    c(
      "/* Sass Bundle */",
      "$color: blue;",
      ".declarations {}",
      ".first {}",
      ".second {}",
      "/* *** */"
    )
  )
})
